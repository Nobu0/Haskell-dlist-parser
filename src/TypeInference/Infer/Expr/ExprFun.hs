module TypeInference.Infer.Expr.ExprFun
  ( inferFunClause,
    inferFunBinding,
  )
where

import AST.Decl (Decl (..), FunClause (..))
import AST.Expr
import AST.Expr (CaseAlt (..), Expr (..), Stmt (..))
import AST.Pattern (Pattern (..))
-- import AST.Type
-- import AST.Type (Type (..))

import qualified AST.Type as AST
import qualified Control.Exception as TypeInference
-- (setTrace)
-- import TypeInference.Types

import Control.Monad (foldM, replicateM, when, zipWithM)
import Data.Bifunctor (first)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import TypeInference.Error
import TypeInference.Infer.Core
-- import TypeInference.Infer.Expr.ExprDispatch (inferExpr)
import TypeInference.Infer.Expr.ExprSQL
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.Type (Type)
import qualified TypeInference.Type as TI
import TypeInference.TypeEnv
import qualified TypeInference.TypeEnv as TypeEnv
import qualified TypeInference.Types as Types
import TypeInference.Unify (unify)
import Utils.MyTrace

inferFunBinding inferExprFn env (PApp (PVar f) args, body) = do
  (s, t) <- inferFunClause inferExprFn env (FunClause args Nothing (Just body) Nothing)
  myTraceE ("<< inferFunBinding: f " ++ show f ++ " t " ++ show t)
  return (s, [(PVar f, t)])

inferFunClause ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  FunClause ->
  InferM (Subst, Type)
inferFunClause inferExprFn env (FunClause pats _mbGuards (Just body) _mbWhere) = do
  (sPats, envPats, patTypes) <- inferPatterns pats
  let envPatApplied = applyEnv sPats envPats
      env'' = mergeEnvs env envPatApplied
  (sBody, tBody) <- inferExprFn env'' body
  myTraceE ("<< inferFunClause: sBody " ++ show sBody ++ " tBody " ++ show tBody)
  let s = composeSubst sBody sPats
      argTypes = map snd patTypes
      funType = foldr TI.TArrow tBody argTypes
  return (s, funType)
inferFunClause _ _ (FunClause _ _ Nothing _) =
  lift $ Left (InferOther "Function clause missing body")

{-}
inferFunClause :: TypeEnv -> FunClause -> InferM (Subst, Type)
inferFunClause env (FunClause pats _mbGuards (Just body) _mbWhere) = do
  (sPats, envPats, argTypes) <- inferPatterns pats
  let envPatApplied = applyEnv sPats envPats
  let env'' = mergeEnvs env envPatApplied
  (sBody, tBody) <- inferExpr env'' body
  myTraceE ("<< inferFunClause: sBody " ++ show sBody ++ " tBody " ++ show tBody)
  -- let env' = mergeEnvs env envPats
  -- (sBody, tBody) <- inferExpr (applyEnv sPats env') body
  let s = composeSubst sBody sPats
  let funType = foldr TI.TArrow tBody argTypes
  return (s, funType)
inferFunClause _ (FunClause _ _ Nothing _) =
  lift $ Left (InferOther "Function clause missing body")
-}
