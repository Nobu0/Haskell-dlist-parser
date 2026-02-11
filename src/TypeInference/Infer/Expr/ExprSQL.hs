module TypeInference.Infer.Expr.ExprSQL (inferSQL) where

import AST.Expr
import AST.Type
import Control.Monad (foldM)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.TypeEnv

inferSQL ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  [Expr] ->
  Either InferError (Subst, Type)
inferSQL inferExprFn env params = do
  (s, _) <- foldM step (emptySubst, env) params
  return (s, TUnit)
  where
    step (sAcc, envAcc) param = do
      (sParam, _) <- inferExprFn envAcc param
      let sNew = sParam `composeSubst` sAcc
      let envNew = applyEnv sNew envAcc
      return (sNew, envNew)
