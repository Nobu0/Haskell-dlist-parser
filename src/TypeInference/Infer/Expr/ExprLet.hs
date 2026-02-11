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
import AST.Type
import Control.Monad (foldM)
import qualified Data.Map as M
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferLet ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Pattern ->
  Expr ->
  Expr ->
  Either InferError (Subst, Type)
inferLet inferExprFn env pat e1 e2 = do
  (sPat, envPat, tPat) <- inferPattern pat
  (s1, t1) <- inferExprFn (applyEnv sPat env) e1
  s2 <- case unify (apply s1 tPat) t1 of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s = s2 `composeSubst` s1 `composeSubst` sPat
  let env' = mergeEnvs (applyEnv s env) (applyEnv s envPat)
  (s3, t2) <- inferExprFn env' e2
  let sFinal = s3 `composeSubst` s
  Right (sFinal, t2)

inferBindings ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  [(Pattern, Expr)] ->
  Either InferError (Subst, TypeEnv)
inferBindings inferExprFn env [] = Right (emptySubst, emptyEnv)
inferBindings inferExprFn env ((pat, expr) : rest) = do
  (sPat, envPat, tPat) <- inferPattern pat
  (sExpr, tExpr) <- inferExprFn (applyEnv sPat env) expr
  sUnify <- case unify (apply sExpr tPat) tExpr of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s = sUnify `composeSubst` sExpr `composeSubst` sPat
  let env' = applyEnv s envPat
  (sRest, envRest) <- inferBindings inferExprFn (applyEnv s env) rest
  let sFinal = sRest `composeSubst` s
  let envFinal = mergeEnvs env' envRest
  Right (sFinal, envFinal)

inferLetBlock ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  [(Pattern, Expr)] ->
  Expr ->
  Either InferError (Subst, Type)
inferLetBlock inferExprFn env binds body = do
  (sBinds, envBinds) <- inferBindings inferExprFn env binds
  let env' = mergeEnvs envBinds env
  let env'' = applyEnv sBinds env'
  inferExprFn env'' body

inferWhere ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [(Pattern, Expr)] ->
  Either InferError (Subst, Type)
inferWhere inferExprFn env e binds = do
  (s1, t1) <- inferExprFn env e
  _ <- foldM (inferBinding inferExprFn) (applyEnv s1 env) binds
  return (s1, t1)

inferBinding ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  (Pattern, Expr) ->
  Either InferError TypeEnv
inferBinding inferExprFn env (pat, expr) = do
  (s1, t1) <- inferExprFn env expr
  (s2, env2, tPat) <- inferPattern pat
  s3 <- case unify t1 tPat of
    Left uerr -> Left (InferUnifyError uerr)
    Right s -> Right s
  let s = s3 `composeSubst` s2 `composeSubst` s1
  let env' = applyEnv s env2
  return env'

-- 同様に inferLetBlock, inferWhere, inferBindings, inferBinding も inferExprFn を引数に取るように変更
{-}
inferLet :: TypeEnv -> Pattern -> Expr -> Expr -> Either InferError (Subst, Type)
inferLet inferExpr env pat e1 e2 = do
  (sPat, envPat, tPat) <- inferPattern pat
  (s1, t1) <- inferExpr (applyEnv sPat env) e1
  s2 <- case unify (apply s1 tPat) t1 of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s = s2 `composeSubst` s1 `composeSubst` sPat
  let env' = mergeEnvs (applyEnv s env) (applyEnv s envPat)
  (s3, t2) <- inferExpr env' e2
  let sFinal = s3 `composeSubst` s
  Right (sFinal, t2)

inferLetBlock :: TypeEnv -> [(Pattern, Expr)] -> Expr -> Either InferError (Subst, Type)
inferLetBlock inferBindings env binds body = do
  (sBinds, envBinds) <- inferBindings env binds
  let env' = mergeEnvs envBinds env
  let env'' = applyEnv sBinds env'
  inferExpr env'' body

inferWhere :: TypeEnv -> Expr -> [(Pattern, Expr)] -> Either InferError (Subst, Type)
inferWhere inferExpr inferBinding env e binds = do
  (s1, t1) <- inferExpr env e
  _ <- foldM inferBinding (applyEnv s1 env) binds
  return (s1, t1)
-}
