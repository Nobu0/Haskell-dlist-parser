module TypeInference.Infer.Expr.ExprDo (inferDo, inferStmt) where

import AST.Expr
-- import AST.Type
import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprLet (inferBindings)
import TypeInference.Infer.Pattern
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
import TypeInference.TypeEnv
import TypeInference.Unify (unify)
import Utils.MyTrace

inferDo ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [Stmt] ->
  InferM (Subst, Type)
inferDo inferExprFn env [] =
  lift $ Left (InferOther "Empty do block")
{-}
inferDo inferExprFn env [ExprStmt e] = do
  myTraceE ("<< inferDo: env " ++ show env ++ " e " ++ show e)
  inferExprFn env e
inferDo inferExprFn env (stmt : rest) = do
  myTraceE ("<< inferDo: env " ++ show env ++ " stmt " ++ show stmt)
  (s1, env1) <- inferStmt inferExprFn env stmt
  let env' = applyEnv s1 env1
  (s2, t2) <- inferDo inferExprFn env' rest
  return (s2 `composeSubst` s1, t2)
-}
inferDo inferExprFn env (Bind pat expr : rest) = do
  (s1, t1) <- inferExprFn env expr
  (s2, env') <- inferPattern' pat t1
  inferDo inferExprFn (mergeEnvs env' env) rest
inferDo inferExprFn env [ExprStmt e] = do
  myTraceE ("<< inferDo: [expr] env " ++ show env ++ " e " ++ show e)
  inferExprFn env e
inferDo inferExprFn env (LetStmt binds : rest) = do
  myTraceE ("<< inferDo: let binds " ++ show binds)
  (s1, env') <- inferBindings inferExprFn env binds
  let env'' = env `mergeEnvs` (applyEnv s1 env')
  myTraceE ("<< inferDo: let rest " ++ show rest)
  (s2, t2) <- inferDo inferExprFn (applyEnv s1 env'') rest
  return (s2 `composeSubst` s1, t2)
inferDo inferExprFn env (ExprStmt e : rest) = do
  myTraceE ("<< inferDo: expr e " ++ show e)
  (s1, _) <- inferExprFn env e
  (s2, t2) <- inferDo inferExprFn (applyEnv s1 env) rest
  return (s2 `composeSubst` s1, t2)

inferStmt ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Stmt ->
  InferM (Subst, TypeEnv)
inferStmt inferExprFn env (ExprStmt e) = do
  (s, _) <- inferExprFn env e
  return (s, env)
inferStmt inferExprFn env (LetStmt binds) =
  inferBindings inferExprFn env binds
inferStmt inferExprFn env (Bind pat e) = do
  (s1, t1) <- inferExprFn env e
  (s2, env2, tPat) <- inferPattern pat
  s3 <- lift $ first InferUnifyError (unify t1 tPat)
  let s = s3 `composeSubst` s2 `composeSubst` s1
  let env' = applyEnv s env2
  return (s, env')
