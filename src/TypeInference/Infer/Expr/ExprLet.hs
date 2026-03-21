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
import Data.Bifunctor (first)
import qualified Data.Map as M
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Pattern
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

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
  (s1, t1) <- inferExprFn env e
  _ <- foldM (inferBinding inferExprFn) (applyEnv s1 env) binds
  return (s1, t1)

inferBinding ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  (Pattern, Expr) ->
  InferM TypeEnv
inferBinding inferExprFn env (pat, expr) = do
  (s1, t1) <- inferExprFn env expr
  (s2, env2, tPat) <- inferPattern pat
  s3 <- lift $ first InferUnifyError (unify t1 tPat)
  let s = s3 `composeSubst` s2 `composeSubst` s1
  let env' = applyEnv s env2
  return env'
