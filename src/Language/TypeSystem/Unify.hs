module Language.TypeSystem.Unify
  ( unify,
    unifyMany,
    unifyListElems,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
-- (TypeLike (..))

-- (composeSubst, emptySubst)

-- import Debug.Trace (trace, traceM, traceShow)
import Language.TypeSystem.BaseType
import Language.TypeSystem.Class
import Language.TypeSystem.Error
import Language.TypeSystem.InferM
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax

-- | 複数の型を順に統一する
unifyMany :: [Type] -> [Type] -> InferM Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  -- s2 <- unifyMany (M.map (applySubst s1) ts1) (M.map (applySubst s1) ts2)
  s2 <- unifyMany (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  return (s2 `composeSubst` s1)
unifyMany t1 t2 =
  throwError $ UnificationMismatch (TTuple t1) (TTuple t2)

unifyListElems :: [Type] -> InferM (Subst, Type)
unifyListElems [] = do
  tv <- freshTypeVar
  return (emptySubst, tv)
unifyListElems (t : ts) = do
  (s1, t1) <- foldM step (emptySubst, t) ts
  return (s1, t1)
  where
    step (s, t1) t2 = do
      s' <- unify (applySubst s t1) (applySubst s t2)
      return (s' `composeSubst` s, applySubst s' t1)

{-}
-- | 型変数に型を束縛する（ただし occurs check を行う）
bindVar :: TVar -> Type -> InferM Subst
bindVar v t
  | t == TVar v = return emptySubst
  | v `Set.member` freeTypeVars t = throwError $ InfiniteType v t
  | otherwise = return $ singletonSubst v t
bindVar :: TVar -> Type -> InferM Subst
-}
bindVar :: Name -> Type -> InferM Subst
bindVar u t
  | t == TVar u = return emptySubst
  | u `Set.member` ftv t = throwError $ InfiniteType u t
  | otherwise = return (M.singleton u t)

-- | 型の統一（t1 と t2 を等しくするための置換を返す）
unify :: Type -> Type -> InferM Subst
unify (TArrow l1 r1) (TArrow l2 r2) = do
  -- traceM ("[unify/TArrow] " ++ show l1 ++ " -> " ++ show r1 ++ " <~> " ++ show l2 ++ " -> " ++ show r2)
  s1 <- unify l1 l2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  return (s2 `composeSubst` s1)
unify (TList t1) (TList t2) =
  unify t1 t2
unify (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyMany ts1 ts2
  | otherwise = throwError $ UnificationFail (TTuple ts1) (TTuple ts2)
unify (TApp f1 x1) (TApp f2 x2) = do
  -- traceM ("[unify/TApp] " ++ show f1 ++ " <~> " ++ show f2)
  s1 <- unify f1 f2
  s2 <- unify (applySubst s1 x1) (applySubst s1 x2)
  return (s2 `composeSubst` s1)
unify (TVar u) t = do
  -- traceM ("[unify/TVar] " ++ show u ++ " <~> " ++ show t)
  bindVar u t
unify t (TVar u) = do
  -- traceM ("[unify/TVar] " ++ show t ++ " <~> " ++ show u)
  bindVar u t
unify t (TVar u) = bindVar u t
unify (TCon c1) (TCon c2)
  | c1 == c2 = return emptySubst
  | otherwise = throwError $ UnificationMismatch (TCon c1) (TCon c2)
unify (TBinOp op1 l1 r1) (TBinOp op2 l2 r2)
  | op1 == op2 = do
      s1 <- unify l1 l2
      s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
      return (s2 `composeSubst` s1)
  | otherwise = throwError $ UnificationMismatch (TBinOp op1 l1 r1) (TBinOp op2 l2 r2)
unify (TRecord f1 (Just r1)) (TRecord f2 (Just r2)) = do
  -- traceM ("[unify/TRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Just " ++ show r1 ++ " | Just " ++ show r2)
  let common = M.intersectionWith (,) f1 f2
      only1 = M.difference f1 f2
      only2 = M.difference f2 f1

  tvs1 <- mapM (const freshTypeVar) (M.toList only1)
  tvs2 <- mapM (const freshTypeVar) (M.toList only2)

  let row1 = TRecord (M.fromList (zip (M.keys only1) tvs1)) (Just r1)
      row2 = TRecord (M.fromList (zip (M.keys only2) tvs2)) (Just r2)

  s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
  s2 <- unify row1 row2
  return (s2 `composeSubst` s1)
unify (TRecord f1 (Just r1)) (TRecord f2 Nothing) = do
  -- traceM ("[unify/TRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Just " ++ show r1 ++ " | Nothing")
  let common = M.intersectionWith (,) f1 f2
      extra = M.difference f2 f1 -- ← ここが重要！f2 にしかないフィールドを row に束縛
  tvs <- mapM (const freshTypeVar) (M.toList extra)
  let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
  s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
  s2 <- case r1 of
    TVar r -> bindVar r extRow
    _ -> throwError $ UnificationFail (TRecord f1 (Just r1)) (TRecord f2 Nothing)
  return (s2 `composeSubst` s1)
unify (TRecord f1 Nothing) (TRecord f2 (Just r2)) = do
  -- traceM ("[unify/TRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Nothing | Just " ++ show r2)
  let common = M.intersectionWith (,) f1 f2
      extra = M.difference f1 f2
  tvs <- mapM (const freshTypeVar) (M.toList extra)
  let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
  s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
  s2 <- case r2 of
    TVar r -> bindVar r extRow
    _ -> throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 (Just r2))
  return (s2 `composeSubst` s1)

{-}
unify (TRecord f1 (Just r1)) (TRecord f2 Nothing) = do
  traceM ("[unify/TRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Just " ++ show r1 ++ " | Nothing")
  let common = M.intersectionWith (,) f1 f2
      extra = M.difference f1 f2
  if M.difference f2 f1 /= M.empty
    then throwError $ UnificationFail (TRecord f1 (Just r1)) (TRecord f2 Nothing)
    else do
      tvs <- mapM (const freshTypeVar) (M.toList extra)
      let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
      s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
      s2 <- case r1 of
        TVar r -> bindVar r extRow
        _ -> throwError $ UnificationFail (TRecord f1 (Just r1)) (TRecord f2 Nothing)
      return (s2 `composeSubst` s1)
unify (TRecord f1 Nothing) (TRecord f2 (Just r2)) = do
  traceM ("[unify/TRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Nothing | Just " ++ show r2)
  let common = M.intersectionWith (,) f1 f2
      extra = M.difference f2 f1
  if M.difference f1 f2 /= M.empty
    then throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 (Just r2))
    else do
      tvs <- mapM (const freshTypeVar) (M.toList extra)
      let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
      s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
      s2 <- case r2 of
        TVar r -> bindVar r extRow
        _ -> throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 (Just r2))
      return (s2 `composeSubst` s1)
-}

unify (TRecord f1 Nothing) (TRecord f2 Nothing) = do
  -- traceM ("[unify/TRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Nothing | Nothing")
  if M.keysSet f1 /= M.keysSet f2
    then throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 Nothing)
    else do
      sps <- mapM (uncurry unify) (M.elems $ M.intersectionWith (,) f1 f2)
      return (foldr composeSubst emptySubst sps)
{-}
unify (TRecord f1 Nothing) (TRecord f2 (Just row)) = do
  traceM ("[unify/TRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | " ++ show row)
  case row of
    TVar r -> do
      let extraFields = M.difference f2 f1
      tvs <- mapM (const freshTypeVar) (M.toList extraFields)
      let extRow = TRecord (M.fromList (zip (M.keys extraFields) tvs)) Nothing
      s1 <- unify (TRecord f1 Nothing) (TRecord (M.union f1 (M.fromList (zip (M.keys extraFields) tvs))) Nothing)
      s2 <- bindVar r extRow
      return (s2 `composeSubst` s1)
    _ -> throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 (Just row))
-- unify (TRecord f1 r1) (TRecord f2 r2) -- unifyRows f1 r1 f2 r2

unify (TRecord f1) (TRecord f2)
  | Map.keysSet f1 == Map.keysSet f2 = unifyMany (Map.elems f1) (Map.elems f2)
  | otherwise = throwError $ UnificationMismatch (TRecord f1) (TRecord f2)
-}
unify TUnit TUnit = return emptySubst
unify t1 t2 = throwError $ UnificationMismatch t1 t2

unifyRows :: M.Map Name Type -> Maybe Type -> M.Map Name Type -> Maybe Type -> InferM Subst
unifyRows f1 r1 f2 r2 = case (r1, r2) of
  (Nothing, Nothing) ->
    if M.keysSet f1 == M.keysSet f2
      then unifyMany (M.elems f1) (M.elems f2)
      else throwError $ UnificationMismatch (TRecord f1 r1) (TRecord f2 r2)
  (Just rv, Nothing) -> do
    let (common, extra1, extra2) = splitFields f1 f2
    s1 <- unifyMany (M.elems common) (mapMaybeFields extra2 f1)
    s2 <- unify rv (TRecord extra1 Nothing)
    return (s2 `composeSubst` s1)
  (Nothing, Just rv) -> do
    let (common, extra2, extra1) = splitFields f2 f1
    s1 <- unifyMany (M.elems common) (mapMaybeFields extra1 f2)
    s2 <- unify rv (TRecord extra2 Nothing)
    return (s2 `composeSubst` s1)
  (Just rv1, Just rv2) -> do
    let (common, rest1, rest2) = splitFields f1 f2
    s1 <- unifyMany (M.elems common) (mapMaybeFields rest2 f1)
    rv <- freshTypeVar
    s2 <- unify rv1 (TRecord rest1 (Just rv))
    s3 <- unify rv2 (TRecord rest2 (Just rv))
    return (s3 `composeSubst` s2 `composeSubst` s1)

splitFields :: (Ord k) => M.Map k a -> M.Map k a -> (M.Map k a, M.Map k a, M.Map k a)
splitFields m1 m2 =
  let commonKeys = M.keysSet m1 `Set.intersection` M.keysSet m2
      common = M.restrictKeys m1 commonKeys
      extra1 = M.withoutKeys m1 commonKeys
      extra2 = M.withoutKeys m2 commonKeys
   in (common, extra1, extra2)

mapMaybeFields :: M.Map Name Type -> M.Map Name Type -> [Type]
mapMaybeFields keys ref = mapMaybe (`M.lookup` ref) (M.keys keys)
