module TypeInference.Infer.Expr.ExprDo (inferDo, inferStmt) where

import AST.Expr
import AST.Type
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprLet (inferBindings)
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferDo ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  [Stmt] ->
  Either InferError (Subst, Type)
inferDo inferExprFn env [] =
  Left (InferOther "Empty do block")
inferDo inferExprFn env [ExprStmt e] =
  inferExprFn env e
inferDo inferExprFn env (stmt : rest) = do
  (s1, env1) <- inferStmt inferExprFn env stmt
  let env' = applyEnv s1 env1
  (s2, t2) <- inferDo inferExprFn env' rest
  return (s2 `composeSubst` s1, t2)

inferStmt ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Stmt ->
  Either InferError (Subst, TypeEnv)
inferStmt inferExprFn env (ExprStmt e) = do
  (s, _) <- inferExprFn env e
  return (s, env)
inferStmt inferExprFn env (LetStmt binds) =
  inferBindings inferExprFn env binds
inferStmt inferExprFn env (Bind pat e) = do
  (s1, t1) <- inferExprFn env e
  (s2, env2, tPat) <- inferPattern pat
  s3 <- case unify t1 tPat of
    Left uerr -> Left (InferUnifyError uerr)
    Right s -> Right s
  let s = s3 `composeSubst` s2 `composeSubst` s1
  let env' = applyEnv s env2
  return (s, env')
