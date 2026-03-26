module TypeInference.Infer.Expr.ExprCase (inferCase) where

import AST.Expr
-- import AST.Type

-- import qualified TypeInference.Type as TI

import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Bifunctor (first)
import qualified Data.Map as Map
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprLet
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.Type
import TypeInference.TypeEnv
import TypeInference.Unify (unify)
import Utils.MyTrace

inferCase ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [CaseAlt] ->
  InferM (Subst, Type)
inferCase inferExprFn env scrut branches = do
  myTraceE ("<< inferCase: scrut " ++ show scrut)
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
inferBranch inferExprFn env tScrut sScrut alt = do
  myTraceE ("<< inferBranch: tScurt " ++ show tScrut ++ " sScrut " ++ show sScrut ++ " alt " ++ show alt)
  case alt of
    CaseAlt pat expr -> do
      (s1, env') <- inferPattern' pat tScrut
      (s2, tBody) <- inferExprFn (mergeEnvs env' env) expr
      return (s2 `composeSubst` s1, tBody)
    CaseAltGuard pat guards -> do
      (s1, env') <- inferPattern' pat tScrut
      results <- forM guards $ \(cond, expr) -> do
        let envBase = mergeEnvs env' env

        -- 条件式の推論（EWhere 対応）
        (sCond, tCond) <- case cond of
          EWhere e binds -> inferWhere inferExprFn envBase e binds
          _ -> inferExprFn envBase cond

        sBool <- lift $ first InferUnifyError $ unify tCond (TCon "Bool")
        let env'' = applyEnv (sBool `composeSubst` sCond) envBase

        -- 結果式の推論（EWhere 対応）
        (sExpr, tExpr) <- case expr of
          EWhere e binds -> inferWhere inferExprFn env'' e binds
          _ -> inferExprFn env'' expr

        let s = sExpr `composeSubst` sBool `composeSubst` sCond
        return (s, apply s tExpr)

      let (subs, types) = unzip results
      sMerged <- lift $ first InferUnifyError $ unifyMany types (replicate (length types) (head types))
      let sTotal = foldr composeSubst sMerged subs
      return (sTotal `composeSubst` s1, apply sTotal (head types))

{-}
inferBranch inferExprFn env tScrut sScrut alt = do
  myTraceE ("<< inferBranch: tScurt " ++ show tScrut ++ " sScrut " ++ show sScrut ++ " alt " ++ show alt)
  case alt of
    CaseAlt pat expr -> do
      (s1, env') <- inferPattern' pat tScrut
      (s2, tBody) <- inferExprFn (mergeEnvs env' env) expr
      return (s2 `composeSubst` s1, tBody)
    CaseAltGuard pat guards -> do
      (s1, env') <- inferPattern' pat tScrut
      results <- forM guards $ \(cond, expr) -> do
        -- 条件式の推論（EWhere に対応）
        (sCond, tCond) <- case cond of
          EWhere e binds -> inferWhere inferExprFn (mergeEnvs env' env) e binds
          _ -> inferExprFn (mergeEnvs env' env) cond

        sBool <- lift $ first InferUnifyError $ unify tCond (TCon "Bool")
        let env'' = applyEnv (sBool `composeSubst` sCond) (mergeEnvs env' env)

        -- 結果式の推論（EWhere に対応）
        (sExpr, tExpr) <- case expr of
          EWhere e binds -> inferWhere inferExprFn env'' e binds
          _ -> inferExprFn env'' expr

        let s = sExpr `composeSubst` sBool `composeSubst` sCond
        return (s, apply s tExpr)
      -- unify all result types
      let (subs, types) = unzip results
      -- sMerged <- unifyMany types
      sMerged <- lift $ first InferUnifyError $ unifyMany types (replicate (length types) (head types))
      let sTotal = foldr composeSubst sMerged subs
      return (sTotal `composeSubst` s1, apply sTotal (head types))
-}
{-}
      results <- forM guards $ \(cond, expr) -> do
        (sCond, tCond) <- inferExprFn (mergeEnvs env' env) cond
        sBool <- lift $ first InferUnifyError $ unify tCond (TCon "Bool")
        let env'' = applyEnv (sBool `composeSubst` sCond) (mergeEnvs env' env)
        (sExpr, tExpr) <- inferExprFn env'' expr
        let s = sExpr `composeSubst` sBool `composeSubst` sCond
        return (s, apply s tExpr)
-}

{-}
inferBranch inferExprFn env tScrut sScrut (CaseAlt pat expr) = do
  (sPat, envPat, tPat) <- inferPattern pat
  sUnify <- lift $ first InferUnifyError (unify (apply sPat tPat) (apply sPat tScrut))
  let s = sUnify `composeSubst` sPat `composeSubst` sScrut
  inferExprFn (applyEnv s (mergeEnvs env envPat)) expr
-}

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
