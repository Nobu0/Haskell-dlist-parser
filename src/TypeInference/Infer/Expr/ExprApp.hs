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

{-}
-- TypeInference/Infer/Expr/ExprApp.hs
module TypeInference.Infer.Expr.ExprApp
  ( inferApp,
    inferLam,
  )
where

import AST.Expr
import AST.Pattern
import AST.Type
import Control.Monad (foldM)
import TypeInference.Error
import TypeInference.Infer.Core
-- import TypeInference.Infer.Expr.CoreExpr (inferExpr)
-- import TypeInference.Infer.Expr.ExprLet (inferBinding, inferBindings)
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferApp :: TypeEnv -> Expr -> Expr -> Either InferError (Subst, Type)
inferApp env e1 e2 = do
  (s1, t1) <- inferExpr env e1
  (s2, t2) <- inferExpr (applyEnv s1 env) e2
  tv <- freshTypeVar
  case unify (apply s2 t1) (TArrow t2 tv) of
    Left _ -> Left (InferMismatch (apply s2 t1) (TArrow t2 tv))
    Right s3 ->
      let s = s3 `composeSubst` s2 `composeSubst` s1
       in Right (s, apply s3 tv)

inferLam :: TypeEnv -> Pattern -> Expr -> Either InferError (Subst, Type)
inferLam env pat body = do
  (s1, env1, tPat) <- inferPattern pat
  (s2, tBody) <- inferExpr (applyEnv s1 (mergeEnvs env env1)) body
  let s = s2 `composeSubst` s1
  Right (s, TArrow (apply s tPat) tBody)
-}