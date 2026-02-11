module TypeInference.Infer.Expr
  ( inferExpr,
    inferProgram,
    inferDecl,
  )
where

import AST.Decl (Decl (..))
import AST.Expr
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
import AST.Pattern (Pattern (..))
import AST.Type
import AST.Type (Type (..))
import qualified Control.Exception as TypeInference
import Control.Monad (foldM)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprDispatch (inferExpr)
import TypeInference.Infer.Expr.ExprSQL
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

-- declsはAST
inferProgram :: TypeEnv -> [Decl] -> Either InferError TypeEnv
inferProgram env decls = do
  let groups = groupDecls decls
  foldM inferGroup env (M.toList groups)

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

inferClause :: TypeEnv -> Decl -> Either InferError (Subst, Type)
inferClause env (DeclFun _ pats body) = do
  (sPats, envPats, argTypes) <- inferPatterns pats
  let env' = mergeEnvs env envPats
  (sBody, tBody) <- inferExpr (applyEnv sPats env') body
  let s = composeSubst sBody sPats
  -- ★ 関数型をここで作る
  let funType = foldr TArrow tBody argTypes
  Right (s, funType)

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
