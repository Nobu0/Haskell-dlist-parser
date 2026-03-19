module TypeInference.Infer.Expr.ExprApp (inferApp) where

import AST.Expr
import AST.Type
import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferApp ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  Expr ->
  InferM (Subst, Type)
inferApp inferExprFn env e1 e2 = do
  (s1, t1) <- inferExprFn env e1
  (s2, t2) <- inferExprFn (applyEnv s1 env) e2
  tv <- freshTypeVar
  s3 <- lift $ first InferUnifyError (unify (apply s2 t1) (TArrow t2 tv))
  let s = s3 `composeSubst` s2 `composeSubst` s1
  return (s, apply s tv)
