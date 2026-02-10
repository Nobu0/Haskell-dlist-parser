module TypeInference.Infer.Expr
  ( inferExpr,
    inferProgram,
    inferDecl,
  )
where

import AST.Decl (Decl (..))
import AST.Expr
-- import TypeInference.TypeEnv
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
-- import AST.Pattern
import AST.Pattern (Pattern (..))
import AST.Type
import AST.Type (Type (..))
import qualified Control.Exception as TypeInference
import Control.Monad (foldM)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
-- import TypeInference.Infer.Expr.CoreExpr (inferExpr)
-- import TypeInference.Infer.Expr.ExprLet (inferBinding, inferBindings)

import Debug.Trace (trace, traceIO, traceShowId)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprDispatch (inferExpr)
import TypeInference.Infer.Expr.ExprSQL
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

freshVar :: Int -> Type
freshVar n = TVar ("t" ++ show n)

-- 宣言の型推論（まだ骨格だけ）
inferDecl :: TypeEnv -> Decl -> Either InferError (TypeEnv, Subst)
inferDecl env decl = case decl of
  DeclTypeSig name ty ->
    let scheme = Forall [] ty
     in Right (extendEnv env name scheme, emptySubst)
  DeclFun name pats body -> do
    -- パターンごとに型推論
    (sPats, envPats, argTypes) <- inferPatterns pats
    -- パターンで拡張した環境で body を推論
    (sBody, tBody) <- inferExpr (applyEnv sPats (mergeEnvs env envPats)) body
    let funType = foldr TArrow tBody argTypes
    let s = composeSubst sBody sPats
    let scheme = generalizeInfer env (apply s funType)
    Right (extendEnv env name scheme, s)
  DeclValue pat expr ->
    Left (InferOther "DeclValue not implemented yet")
  _ ->
    Right (env, emptySubst)

inferProgram :: TypeEnv -> [Decl] -> Either InferError TypeEnv
inferProgram env decls = do
  let groups = groupDecls decls
  foldM inferGroup env (M.toList groups)

inferClause :: TypeEnv -> Decl -> Either InferError (Subst, Type)
inferClause env (DeclFun _ pats body) = do
  (sPats, envPats, argTypes) <- inferPatterns pats
  let env' = mergeEnvs env envPats
  (sBody, tBody) <- inferExpr (applyEnv sPats env') body
  let s = composeSubst sBody sPats
  -- ★ 関数型をここで作る
  let funType = foldr TArrow tBody argTypes
  Right (s, funType)

inferGroup :: TypeEnv -> (Name, [Decl]) -> Either InferError TypeEnv
inferGroup env (name, clauses) = do
  -- 仮の型を環境に入れる（再帰対応）
  let tempType = TVar ("t_fun_" ++ name)
  let envTemp = extendEnv env name (Forall [] tempType)
  -- 各 clause の型を推論
  inferred <- mapM (inferClause envTemp) clauses
  let funTypes = [apply s t | (s, t) <- inferred]
  -- unify して 1 つの型にまとめる
  s <- unifyMany funTypes
  let finalType = apply s (head funTypes)
  -- generalize
  let scheme = generalizeInfer env finalType
  Right (extendEnv env name scheme)

inferBindings :: TypeEnv -> [(Pattern, Expr)] -> Either InferError (Subst, TypeEnv)
inferBindings env [] = Right (emptySubst, emptyEnv)
inferBindings env ((pat, expr) : rest) = do
  -- パターン推論
  (sPat, envPat, tPat) <- inferPattern pat
  -- 右辺の推論
  (sExpr, tExpr) <- inferExpr (applyEnv sPat env) expr
  -- パターン型と右辺型を unify（エラー型を変換）
  sUnify <- case unify (apply sExpr tPat) tExpr of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s = sUnify `composeSubst` sExpr `composeSubst` sPat
  let env' = applyEnv s envPat
  -- 残りの束縛
  (sRest, envRest) <- inferBindings (applyEnv s env) rest
  let sFinal = sRest `composeSubst` s
  let envFinal = mergeEnvs env' envRest
  Right (sFinal, envFinal)

inferBinding :: TypeEnv -> (Pattern, Expr) -> Either InferError TypeEnv
inferBinding env (pat, expr) = do
  -- 式の型を推論
  (s1, t1) <- inferExpr env expr
  -- パターンの型を推論
  (s2, env2, tPat) <- inferPattern pat
  -- unify のエラーを InferError に変換
  s3 <- case unify t1 tPat of
    Left uerr -> Left (InferUnifyError uerr)
    Right s -> Right s
  -- 置換を合成
  let s = s3 `composeSubst` s2 `composeSubst` s1
  -- 環境に置換を適用
  let env' = applyEnv s env2
  return env'

unifyList :: Type -> [(Subst, Type)] -> Either InferError Subst
unifyList t [] = Right emptySubst
unifyList t ((s, tElem) : rest) = do
  sU <- case unify (apply s t) tElem of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s' = sU `composeSubst` s
  unifyList (apply s' t) rest
