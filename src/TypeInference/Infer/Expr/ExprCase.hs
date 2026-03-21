module TypeInference.Infer.Expr.ExprCase (inferCase) where

import AST.Expr
-- import AST.Type
import Control.Monad (foldM)
import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Pattern
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferCase ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [CaseAlt] ->
  InferM (Subst, Type)
inferCase inferExprFn env scrut branches = do
  (sScrut, tScrut) <- inferExprFn env scrut
  results <- mapM (inferBranch inferExprFn env tScrut sScrut) branches
  unifyManyExpr results

inferBranch ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Type ->
  Subst ->
  CaseAlt ->
  InferM (Subst, Type)
inferBranch inferExprFn env tScrut sScrut (CaseAlt pat expr) = do
  (sPat, envPat, tPat) <- inferPattern pat
  sUnify <- lift $ first InferUnifyError (unify (apply sPat tPat) (apply sPat tScrut))
  let s = sUnify `composeSubst` sPat `composeSubst` sScrut
  inferExprFn (applyEnv s (mergeEnvs env envPat)) expr

unifyManyExpr :: [(Subst, Type)] -> InferM (Subst, Type)
unifyManyExpr [] = lift $ Left (InferOther "empty case")
unifyManyExpr ((s, t) : xs) = foldM step (s, t) xs
  where
    step (sAcc, tAcc) (sNext, tNext) = do
      sU <- case unify (apply sAcc tAcc) (apply sAcc tNext) of
        Left uerr -> lift $ Left (InferUnifyError uerr)
        Right su -> return su
      let sFinal = sU `composeSubst` sNext `composeSubst` sAcc
      return (sFinal, apply sFinal tAcc)
