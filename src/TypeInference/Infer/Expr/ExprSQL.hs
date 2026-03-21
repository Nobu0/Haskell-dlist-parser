module TypeInference.Infer.Expr.ExprSQL (inferSQL) where

import AST.Expr
-- import AST.Type
import Control.Monad (foldM)
import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
import TypeInference.TypeEnv

inferSQL ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [Expr] ->
  InferM (Subst, Type)
inferSQL inferExprFn env params = do
  (s, _) <- foldM step (emptySubst, env) params
  return (s, TUnit)
  where
    step (sAcc, envAcc) param = do
      (sParam, _) <- inferExprFn envAcc param
      let sNew = sParam `composeSubst` sAcc
      let envNew = applyEnv sNew envAcc
      return (sNew, envNew)
