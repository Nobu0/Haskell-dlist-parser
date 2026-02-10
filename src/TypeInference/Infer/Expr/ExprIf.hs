module TypeInference.Infer.Expr.ExprIf
  ( inferIf,
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

inferIf ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  Expr ->
  Expr ->
  Either InferError (Subst, Type)
inferIf inferExprFn env cond eThen eElse = do
  (s1, tCond) <- inferExprFn env cond
  sBool <- case unify tCond (TCon "Bool") of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let env1 = applyEnv (sBool `composeSubst` s1) env
  (s2, tThen) <- inferExprFn env1 eThen
  (s3, tElse) <- inferExprFn (applyEnv s2 env1) eElse
  s4 <- case unify (apply s3 tThen) tElse of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  Right (s, apply s4 tElse)

{-}
inferIf inferExprFn env cond eThen eElse = do
  (s1, tCond) <- inferExprFn env cond
  sBool <- case unify tCond (TCon "Bool") of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let env1 = applyEnv (sBool `composeSubst` s1) env
  (s2, tThen) <- inferExprFn env1 eThen
  (s3, tElse) <- inferExprFn (applyEnv s2 env1) eElse
  s4 <- case unify (apply s3 tThen) tElse of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  Right (s, apply s4 tElse)
-}