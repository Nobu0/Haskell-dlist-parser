module TypeInference.Infer.Expr.ExprLet
  ( inferLet,
    inferLetBlock,
    inferWhere,
    inferBindings,
    inferBinding,
  )
where

import AST.Expr
import AST.Pattern
-- import AST.Type
import Control.Monad (foldM)
-- import qualified TypeInference.Type as TI

-- import TypeInference.Types

import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bifunctor (first)
import qualified Data.Map as M
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.Type (Type (..))
import TypeInference.TypeEnv
import TypeInference.Unify (unify)
import Utils.MyTrace (myTraceE)

inferLet ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Pattern ->
  Expr ->
  Expr ->
  InferM (Subst, Type)
inferLet inferExprFn env pat e1 e2 = do
  (sPat, envPat, tPat) <- inferPattern pat
  (s1, t1) <- inferExprFn (applyEnv sPat env) e1
  s2 <- lift $ first InferUnifyError (unify (apply s1 tPat) t1)
  let s = s2 `composeSubst` s1 `composeSubst` sPat
  let env' = mergeEnvs (applyEnv s env) (applyEnv s envPat)
  (s3, t2) <- inferExprFn env' e2
  let sFinal = s3 `composeSubst` s
  return (sFinal, t2)

inferBindings ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [(Pattern, Expr)] ->
  InferM (Subst, TypeEnv)
inferBindings inferExprFn env [] = return (emptySubst, emptyEnv)
inferBindings inferExprFn env ((pat, expr) : rest) = do
  (sPat, envPat, tPat) <- inferPattern pat
  (sExpr, tExpr) <- inferExprFn (applyEnv sPat env) expr
  sUnify <- lift $ first InferUnifyError (unify (apply sExpr tPat) tExpr)
  let s = sUnify `composeSubst` sExpr `composeSubst` sPat
  let env' = applyEnv s envPat
  (sRest, envRest) <- inferBindings inferExprFn (applyEnv s env) rest
  let sFinal = sRest `composeSubst` s
  let envFinal = mergeEnvs env' envRest
  return (sFinal, envFinal)

inferLetBlock ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [(Pattern, Expr)] ->
  Expr ->
  InferM (Subst, Type)
inferLetBlock inferExprFn env binds body = do
  (sBinds, envBinds) <- inferBindings inferExprFn env binds
  let env' = mergeEnvs envBinds env
  let env'' = applyEnv sBinds env'
  inferExprFn env'' body

inferWhere ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [(Pattern, Expr)] ->
  InferM (Subst, Type)
inferWhere inferExprFn env e binds = do
  myTraceE $ "<< inferWhere:* binds count = " ++ show (length binds)

  -- 1. binds を先に処理して環境に追加！
  (s1, env') <-
    foldM
      ( \(sAcc, envAcc) (pat, expr) -> do
          -- myTraceE $ "<< inferBinding: pat = " ++ show pat
          (sNew, envNext) <- inferBinding inferExprFn envAcc (pat, expr)
          let sCombined = sNew `composeSubst` sAcc
          return (sCombined, envNext)
      )
      (emptySubst, env)
      binds

  -- 2. 拡張された環境で本体を推論！
  (s2, t2) <- inferExprFn (applyEnv s1 env') e

  return (s2 `composeSubst` s1, t2)

{-}
inferWhere inferExprFn env e binds = do
  myTraceE $ "<< inferWhere:* binds " ++ show binds
  myTraceE $ "<< inferWhere:* binds count = " ++ show (length binds)
  (s1, t1) <- inferExprFn env e
  -- ここで foldM の結果を使うようにする！
  (sFinal, env') <-
    foldM
      ( \(sAcc, envAcc) (pat, expr) -> do
          myTraceE $ "<< inferBinding: pat = " ++ show pat
          (sNew, envNext) <- inferBinding inferExprFn envAcc (pat, expr)
          let sCombined = sNew `composeSubst` sAcc
          return (sCombined, envNext)
      )
      (s1, applyEnv s1 env)
      binds

  -- 拡張された環境で再度本体を推論してもよい（必要なら）
  -- (s2, t2) <- inferExprFn env'
  -- return (s2 `composeSubst` sFinal, t2)

  -- 今は元の推論結果を返す
  return (sFinal, t1)
-}

inferBinding ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  (Pattern, Expr) ->
  InferM (Subst, TypeEnv)
inferBinding inferExprFn env (pat, expr) = do
  myTraceE $ "<< inferBinding: pat = " ++ show pat

  -- 1. パターンから型と環境を推論
  (sPat, envFromPat, patType) <- inferPattern pat

  -- 2. 式を推論
  (sExpr, exprType) <- inferExprFn (applyEnv sPat envFromPat) expr

  -- 3. パターンの型と式の型を unify
  -- sUnify <- unify (apply sExpr patType) exprType
  sUnify <- liftEither (mapLeft InferUnifyError $ unify (apply sExpr patType) exprType)

  -- 4. 環境を構築
  let sAll = sUnify `composeSubst` sExpr `composeSubst` sPat
  let finalEnv = applyEnv sAll envFromPat
  let name = nameFromPat pat
  let scheme = generalize (applyEnv sAll env) (apply sAll exprType)
  let env' = extendEnv env name scheme

  -- myTraceE $ ">> binding name = " ++ name ++ ", scheme = " ++ show scheme
  return (sAll, env')

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left e) = Left (f e)
mapLeft _ (Right x) = Right x

{-}
nameFromPat :: Pattern -> Name
nameFromPat (PVar name) = name
nameFromPat (PApp (PVar name) _) = name
nameFromPat (PApp (PConstr name _) _) = name
nameFromPat (PApp pat _) = nameFromPat pat
nameFromPat (PCons pat1 _) = nameFromPat pat1
nameFromPat pat = error $ "nameFromPat: unsupported pattern " ++ show pat
-}

nameFromPat :: Pattern -> Name
nameFromPat (PVar name) = name
nameFromPat (PApp (PVar name) _) = name
nameFromPat (PApp pat _) = nameFromPat pat
nameFromPat (PCons pat1 _) = nameFromPat pat1
nameFromPat pat = error $ "nameFromPat: unsupported pattern " ++ show pat
