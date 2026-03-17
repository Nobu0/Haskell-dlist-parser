module TypeInference.Infer.Expr.ExprApp (inferApp) where

import AST.Expr
import AST.Type
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferApp ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  Expr ->
  Either InferError (Subst, Type)
inferApp inferExprFn env e1 e2 = do
  (s1, t1) <- inferExprFn env e1
  (s2, t2) <- inferExprFn (applyEnv s1 env) e2
  tv <- freshTypeVar
  s3 <- case unify (apply s2 t1) (TFun t2 tv) of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s = s3 `composeSubst` s2 `composeSubst` s1
  Right (s, apply s tv)
