module Language.TypeSystem.Unify
  ( unify,
    -- unifyMany,
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

import Language.TypeSystem.BaseType
import Language.TypeSystem.Class
import Language.TypeSystem.Error
import Language.TypeSystem.InferM
import Language.TypeSystem.MyTrace (myTraceE)
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax

-- import Utils.MyTrace (myTraceE)

-- 補助関数：Map同士を安全に単一化
unifyManyG :: M.Map Name Type -> M.Map Name Type -> InferM Subst
unifyManyG m1 m2 = do
  let types = zip (M.elems m1) (M.elems m2) -- commonKeys から作るので要素数は一致
  foldM
    ( \s (t1, t2) -> do
        s' <- unify (applySubst s t1) (applySubst s t2)
        return (s' `composeSubst` s)
    )
    emptySubst
    types

-- | 複数の型を順に統一する
unifyMany :: [Type] -> [Type] -> InferM Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  -- s2 <- unifyoMany (M.map (applySubst s1) ts1) (M.map (applySubst s1) ts2)
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

unify :: Type -> Type -> InferM Subst
unify t1 t2 = do
  myTraceE ("[unify] in >>" ++ show t1 ++ " <~> " ++ show t2)
  e <- unify2 t1 t2
  myTraceE ("[unify] ot <<" ++ show e)
  return e

-- | 型の統一（t1 と t2 を等しくするための置換を返す）
unify2 :: Type -> Type -> InferM Subst
unify2 (TArrow l1 r1) (TArrow l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  return (s2 `composeSubst` s1)
unify2 (TList t1) (TList t2) =
  unify t1 t2
unify2 (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyMany ts1 ts2
  | otherwise = throwError $ UnificationFail (TTuple ts1) (TTuple ts2)
unify2 (TApp f1 x1) (TApp f2 x2) = do
  s1 <- unify f1 f2
  s2 <- unify (applySubst s1 x1) (applySubst s1 x2)
  return (s2 `composeSubst` s1)
unify2 (TVar u) t = bindVar u t
unify2 t (TVar u) = bindVar u t
unify2 (TCon c1) (TCon c2)
  | c1 == c2 = return emptySubst
  | otherwise = throwError $ UnificationMismatch (TCon c1) (TCon c2)
unify2 (TBinOp op1 l1 r1) (TBinOp op2 l2 r2)
  | op1 == op2 = do
      s1 <- unify l1 l2
      s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
      return (s2 `composeSubst` s1)
  | otherwise = throwError $ UnificationMismatch (TBinOp op1 l1 r1) (TBinOp op2 l2 r2)
unify2 (TRecord f1 r1) (TRecord f2 r2) = do
  -- myTraceE ("[unifyRecordG] " ++ show f1 ++ " <~> " ++ show f2 ++ " | r1 = " ++ show r1 ++ " | r2 = " ++ show r2)
  unifyRecordG (f1, r1) (f2, r2)
unify2 TUnit TUnit = return emptySubst
unify2 t1 t2 = throwError $ UnificationMismatch t1 t2

-- | レコードの単一化
unifyRecordG :: (M.Map Name Type, Maybe Type) -> (M.Map Name Type, Maybe Type) -> InferM Subst
unifyRecordG (f1, r1) (f2, r2) = do
  let commonKeys = M.intersectionWith (,) f1 f2
      extra1 = M.difference f1 f2 -- f1にしかない
      extra2 = M.difference f2 f1 -- f2にしかない

  -- 1. 共通フィールドの型を単一化
  -- M.elems を使う前に intersectionWith でペアを作っているので順序問題が解消されます
  s1 <- unifyManyG (M.map fst commonKeys) (M.map snd commonKeys)

  -- 2. 残ったフィールド (extra1, extra2) と行変数 (r1, r2) の処理
  case (r1, r2) of
    -- 両方閉じている場合：残ったフィールドがあってはいけない
    (Nothing, Nothing) ->
      if M.null extra1 && M.null extra2
        then return s1
        else throwError $ UnificationMismatch (TRecord f1 r1) (TRecord f2 r2)
    -- 左側だけ開いている場合：r1 が「f2側の余り」を吸収する
    (Just (TVar rv1), Nothing) -> do
      if not (M.null extra1)
        then throwError $ UnificationMismatch (TRecord f1 r1) (TRecord f2 r2)
        else do
          let extRow = TRecord extra2 Nothing
          s2 <- bindVar rv1 (applySubst s1 extRow)
          return (s2 `composeSubst` s1)

    -- 右側だけ開いている場合：r2 が「f1側の余り」を吸収する
    (Nothing, Just (TVar rv2)) -> do
      if not (M.null extra2)
        then throwError $ UnificationMismatch (TRecord f1 r1) (TRecord f2 r2)
        else do
          let extRow = TRecord extra1 Nothing
          s2 <- bindVar rv2 (applySubst s1 extRow)
          return (s2 `composeSubst` s1)

    -- 両方開いている場合：新しい行変数を作って統合する (Row Polymorphism の核心)
    (Just (TVar rv1), Just (TVar rv2)) -> do
      rv3 <- freshTypeVar -- 新しい行変数
      -- s2: rv1 は「右側にしかないもの + 新しい行変数」
      s2 <- bindVar rv1 (TRecord extra2 (Just rv3))
      -- s3: rv2 は「左側にしかないもの + 新しい行変数」
      let s12 = s2 `composeSubst` s1
      s3 <- bindVar rv2 (applySubst s12 (TRecord extra1 (Just rv3)))
      return (s3 `composeSubst` s12)
    _ -> throwError $ UnificationFail (TRecord f1 r1) (TRecord f2 r2)

{-}
unifyRecord :: (M.Map Name Type, Maybe Type) -> (M.Map Name Type, Maybe Type) -> InferM Subst
unifyRecord (f1, r1) (f2, r2) = do
  -- myTraceE ("[unifyRecord] " ++ show f1 ++ " <~> " ++ show f2 ++ " | r1 = " ++ show r1 ++ " | r2 = " ++ show r2)
  case r1 of
    Just rv1 -> do
      case r2 of
        Just rv2 -> do
          -- myTraceE ("[unifyRecord/1] " ++ show f1 ++ " <~> " ++ show f2 ++ " | rv1 = " ++ show rv1 ++ " | rv2 = " ++ show rv2)
          let (common, rest1, rest2) = splitFields f1 f2
          s1 <- unifyMany (M.elems common) (mapMaybeFields rest2 f1)
          rv <- freshTypeVar
          s2 <- unify rv1 (TRecord rest1 (Just rv))
          s3 <- unify rv2 (TRecord rest2 (Just rv))
          return (s3 `composeSubst` s2 `composeSubst` s1)
        Nothing -> do
          -- myTraceE ("[unifyRecord/2] " ++ show f1 ++ " <~> " ++ show f2 ++ " | rv1 = " ++ show rv1 ++ " | r2 = " ++ show r2)
          let common = M.intersectionWith (,) f1 f2
              extra = M.difference f2 f1 -- ← ここが重要！f2 にしかないフィールドを row に束縛
          tvs <- mapM (const freshTypeVar) (M.toList extra)
          let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
          s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
          s2 <- case rv1 of
            TVar r -> bindVar r extRow
            _ -> throwError $ UnificationFail (TRecord f1 (Just rv1)) (TRecord f2 Nothing)
          return (s2 `composeSubst` s1)
    Nothing -> do
      case r2 of
        Just rv2 -> do
          -- myTraceE ("[unifyRecord/3] " ++ show f1 ++ " <~> " ++ show f2 ++ " | r1 = " ++ show r1 ++ " | rv2 = " ++ show rv2)
          let common = M.intersectionWith (,) f1 f2
              extra = M.difference f1 f2
          tvs <- mapM (const freshTypeVar) (M.toList extra)
          let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
          s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
          s2 <- case rv2 of
            TVar r -> bindVar r extRow
            _ -> throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 (Just rv2))
          return (s2 `composeSubst` s1)
        Nothing -> do
          -- myTraceE ("[unifyRecord/4] " ++ show f1 ++ " <~> " ++ show f2 ++ " | r1 = " ++ show r1 ++ " | r2 = " ++ show r2)
          if M.keysSet f1 == M.keysSet f2
            then unifyMany (M.elems f1) (M.elems f2)
            else throwError $ UnificationMismatch (TRecord f1 r1) (TRecord f2 r2)
-}

splitFields :: (Ord k) => M.Map k a -> M.Map k a -> (M.Map k a, M.Map k a, M.Map k a)
splitFields m1 m2 =
  let commonKeys = M.keysSet m1 `Set.intersection` M.keysSet m2
      common = M.restrictKeys m1 commonKeys
      extra1 = M.withoutKeys m1 commonKeys
      extra2 = M.withoutKeys m2 commonKeys
   in (common, extra1, extra2)

mapMaybeFields :: M.Map Name Type -> M.Map Name Type -> [Type]
mapMaybeFields keys ref = mapMaybe (`M.lookup` ref) (M.keys keys)

{-}
unifyRecord (TRecord f1 (Just r1)) (TRecord f2 (Just r2)) = do
  myTraceE ("[unify2/TRecord/1] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Just " ++ show r1 ++ " | Just " ++ show r2)
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
unifyRecord (TRecord f1 (Just r1)) (TRecord f2 Nothing) = do
  myTraceE ("[unifyRecord/TRecord/2] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Just " ++ show r1 ++ " | Nothing")
  let common = M.intersectionWith (,) f1 f2
      extra = M.difference f2 f1 -- ← ここが重要！f2 にしかないフィールドを row に束縛
  tvs <- mapM (const freshTypeVar) (M.toList extra)
  let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
  s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
  s2 <- case r1 of
    TVar r -> bindVar r extRow
    _ -> throwError $ UnificationFail (TRecord f1 (Just r1)) (TRecord f2 Nothing)
  return (s2 `composeSubst` s1)
unifyRecord (TRecord f1 Nothing) (TRecord f2 (Just r2)) = do
  myTraceE ("[unifyRecord/TRecord/3] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Nothing | Just " ++ show r2)
  let common = M.intersectionWith (,) f1 f2
      extra = M.difference f1 f2
  tvs <- mapM (const freshTypeVar) (M.toList extra)
  let extRow = TRecord (M.fromList (zip (M.keys extra) tvs)) Nothing
  s1 <- unify (TRecord (M.map fst common) Nothing) (TRecord (M.map snd common) Nothing)
  s2 <- case r2 of
    TVar r -> bindVar r extRow
    _ -> throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 (Just r2))
  return (s2 `composeSubst` s1)
unifyRecord (TRecord f1 Nothing) (TRecord f2 Nothing) = do
  myTraceE ("[unify2/TRecord/4] " ++ show f1 ++ " <~> " ++ show f2 ++ " | Nothing | Nothing")
  if M.keysSet f1 /= M.keysSet f2
    then throwError $ UnificationFail (TRecord f1 Nothing) (TRecord f2 Nothing)
    else do
      sps <- mapM (uncurry unify2) (M.elems $ M.intersectionWith (,) f1 f2)
      return (foldr composeSubst emptySubst sps)
-}
