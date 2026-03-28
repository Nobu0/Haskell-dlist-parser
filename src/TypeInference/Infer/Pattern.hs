module TypeInference.Infer.Pattern
  ( inferPattern,
    inferPattern',
    inferPatterns,
    inferPatternApp,
    unifyM,
    unifyManyM,
    unifyBindings,
    generalizeAll,
  )
where

import AST.Decl (Decl (..))
-- import TypeInference.TypeEnv
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
import AST.Pattern (Pattern (..))
-- import AST.Type (Type (..))
-- import TypeInference.SQLInfer

import qualified AST.Type as AST
-- import Data.Bifunctor (first)

-- import Control.Arrow (first)
import qualified Control.Exception as TypeInference
import Control.Monad (foldM, replicateM)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first, second)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Error (InferError (..))
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.Type
import qualified TypeInference.Type as TI
import TypeInference.TypeEnv
import TypeInference.Unify (UnifyError (..), unify, unifyMany)
import qualified TypeInference.Unify as Unify
import Utils.MyTrace

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft = first

generalizeAll :: TypeEnv -> [(Pattern, Type)] -> [(Pattern, Scheme)]
generalizeAll env binds =
  [ (p, generalize env t)
    | (p, t) <- binds
  ]

unifyManyM :: [Type] -> InferM Type
unifyManyM [] = lift $ Left (InferOther "Cannot unify empty type list")
unifyManyM (t : ts) =
  if all (== t) ts
    then return t
    else lift $ Left (InferMismatchGroup (t : ts))

inferPattern :: Pattern -> InferM (Subst, TypeEnv, Type)
inferPattern pat = do
  myTraceE ("<< infrePattern: pat " ++ show pat)
  tv <- freshTypeVar
  (r1, r2) <- inferPattern' pat tv
  return (r1, r2, tv)

inferPattern' :: Pattern -> Type -> InferM (Subst, TypeEnv)
inferPattern' (PApp (PAs name f) args) expectedType = do
  tv <- freshTypeVar
  (s1, env1) <- inferPattern' (PApp f args) tv
  myTraceE ("<< inferPattern: PApp,PAs s1 " ++ show s1 ++ " env1 " ++ show env1)
  s2 <- liftEither (mapLeft InferUnifyError $ unify (apply s1 tv) expectedType)
  let t = apply s2 (apply s1 tv)
      env2 = extendEnv (applyEnv s2 env1) name (Forall [] t)
  return (s2 `composeSubst` s1, env2)
-- As パターン x@p
inferPattern' (PAs name pat) expectedType = do
  myTraceE ("<< inferPattern: PAs " ++ name)
  (s1, env1) <- inferPattern' pat expectedType
  let t = apply s1 expectedType
      env2 = extendEnv env1 name (Forall [] t)
  myTraceE ("<< inferPattern: PAs s1 " ++ show s1 ++ " env1 " ++ show env1 ++ " env2 " ++ show env2)
  return (s1, env2)
-- 変数パターン
inferPattern' (PVar x) expectedType = do
  let env = extendEnv emptyEnv x (Forall [] expectedType)
  return (emptySubst, env)
-- 関数適用パターン
inferPattern' (PApp fnPat args) expectedType = do
  retType <- freshTypeVar
  (sArgs, envArgs, argTypes) <- inferPatterns args
  let fnExpectedType = foldr TArrow expectedType (map snd argTypes)
  (sFn, envFn) <- inferPattern' fnPat fnExpectedType
  let sAll = sArgs `composeSubst` sFn
      envCombined = mergeEnvs (applyEnv sArgs envFn) envArgs
  return (sAll, applyEnv sAll envCombined)
-- 整数リテラル
inferPattern' (PInt _) expectedType = do
  s <- unifyM expectedType (TCon "Int")
  return (s, emptyEnv)
-- ワイルドカード
inferPattern' PWildcard expectedType = do
  return (emptySubst, emptyEnv)
-- リストパターン
inferPattern' (PList ps) expectedType = do
  tv <- freshTypeVar
  (s, env, pts) <- inferPatterns ps
  sList <- unifyM expectedType (TList tv)
  let ts = map snd pts
  case ts of
    [] -> return (sList `composeSubst` s, env)
    (t0 : _) -> do
      s' <-
        foldM
          ( \sacc t ->
              case unify (apply sacc t) (apply sacc t0) of
                Left _ -> lift $ Left (InferMismatch (apply sacc t) (apply sacc t0))
                Right s -> return (composeSubst s sacc)
          )
          s
          ts
      sElem <- unifyM (apply s' t0) (apply s' tv)
      let sAll = sElem `composeSubst` s' `composeSubst` sList
      return (sAll, applyEnv sAll env)
-- タプルパターン
inferPattern' (PTuple ps) expectedType = do
  tvs <- replicateM (length ps) freshTypeVar
  sTuple <- unifyM expectedType (TTuple tvs)
  (sPats, env, pts) <- inferPatterns ps
  let ts = map snd pts
  sUnify <- lift $ first InferUnifyError (unifyMany ts tvs)
  let sAll = sUnify `composeSubst` sPats `composeSubst` sTuple
  return (sAll, applyEnv sAll env)

-- コンストラクタパターン
inferPattern' (PConstr con args) expectedType = do
  case lookupEnv builtinPatternEnv con of
    Nothing -> lift $ Left (InferOther ("Unknown constructor: " ++ con))
    Just scheme -> do
      tCon <- lift $ instantiate scheme
      inferPatternApp' tCon args expectedType
-- Cons パターン (x:xs)

inferPattern' (PCons p1 p2) expectedType = do
  tv <- freshTypeVar
  sList <- unifyM expectedType (TList tv)
  (s1, env1) <- inferPattern' p1 tv
  (s2, env2) <- inferPattern' p2 (TList tv)
  myTraceE ("<< inferPattern: PCons s1 " ++ show s1 ++ " s2 " ++ show s2)
  let s = s2 `composeSubst` s1 `composeSubst` sList
      env = mergeEnvs (applyEnv s2 env1) env2
  return (s, applyEnv s env)
{-}
inferPattern' (PCons p1 p2) expectedType = do
  tv <- freshTypeVar
  (s1, env1) <- inferPattern' p1 tv
  (s2, env2) <- inferPattern' (apply s1 p2) (TList (apply s1 tv))
  myTraceE ("<< inferPattern: PCons s1 " ++ show s1 ++ " s2 " ++ show s2)
  return (s2 `composeSubst` s1, env1 `mergeEnvs` env2)
-}
inferPattern' (PChar _) expectedType = do
  s <- liftEither (mapLeft InferUnifyError $ unify expectedType (TCon "Char"))
  return (s, emptyEnv)
inferPattern' (PInfix p1 op p2) expectedType =
  inferPattern' (PApp (PApp (PVar op) [p1]) [p2]) expectedType

unifyM :: Type -> Type -> InferM Subst
unifyM t1 t2 =
  case Unify.unify t1 t2 of
    Left err -> throwError (InferUnifyError err)
    Right s -> return s

inferPatternApp' :: Type -> [Pattern] -> Type -> InferM (Subst, TypeEnv)
inferPatternApp' tCon pat tv = do
  (r1, r2, _) <- inferPatternApp tCon pat
  return (r1, r2)

inferPatternApp :: Type -> [Pattern] -> InferM (Subst, TypeEnv, Type)
inferPatternApp tCon [] =
  return (emptySubst, emptyEnv, tCon)
inferPatternApp tCon (p : ps) = do
  (s1, env1, tArg) <- inferPattern p
  let alpha = TVar "t_app"
  s2 <- case unify (apply s1 tCon) (TArrow tArg alpha) of
    Left _ -> lift $ Left (InferMismatch (apply s1 tCon) (TArrow tArg alpha))
    Right s -> return s
  (s3, env2, tRes) <- inferPatternApp (apply s2 alpha) ps
  let s = composeSubst s3 (composeSubst s2 s1)
      env = mergeEnvs env1 env2
  return (s, env, apply s tRes)

inferPatterns :: [Pattern] -> InferM (Subst, TypeEnv, [(Pattern, Type)])
inferPatterns [] = return (emptySubst, emptyEnv, [])
inferPatterns (p : ps) = do
  (s1, env1, t1) <- inferPattern p
  (s2, env2, pts) <- inferPatterns ps
  let s = composeSubst s2 s1
      env = mergeEnvs env1 env2
  return (s, env, (p, t1) : pts)

unifyBindings ::
  [(Pattern, Type)] -> -- パターンに割り当てた型
  [(Pattern, Type)] -> -- 推論された右辺の型
  InferM Subst
unifyBindings [] [] = return emptySubst
unifyBindings ((p1, t1) : ps1) ((p2, t2) : ps2) = do
  s1 <- unifyM t1 t2
  let ps1' = map (second (apply s1)) ps1
  let ps2' = map (second (apply s1)) ps2
  s2 <- unifyBindings ps1' ps2'
  return (s2 `composeSubst` s1)
unifyBindings _ _ =
  throwError (InferOther "unifyBindings: mismatched binding lengths")
