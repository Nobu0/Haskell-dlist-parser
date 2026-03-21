module TypeInference.Infer.Expr.ExprIf
  ( inferIf,
  )
where

import AST.Expr
-- import AST.Pattern
-- import AST.Type
import Control.Monad (foldM)
-- import TypeInference.Infer.Expr.CoreExpr (inferExpr)
-- import TypeInference.Infer.Expr.ExprLet (inferBinding, inferBindings)

import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Pattern
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferIf ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  Expr ->
  Expr ->
  InferM (Subst, Type)
inferIf inferExprFn env cond eThen eElse = do
  (s1, tCond) <- inferExprFn env cond
  sBool <- lift $ first InferUnifyError (unify tCond (TCon "Bool"))
  let env1 = applyEnv (sBool `composeSubst` s1) env
  (s2, tThen) <- inferExprFn env1 eThen
  (s3, tElse) <- inferExprFn (applyEnv s2 env1) eElse
  s4 <- lift $ first InferUnifyError (unify (apply s3 tThen) tElse)
  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  return (s, apply s4 tElse)
