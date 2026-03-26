module TypeInference.Unify (Subst, unify, UnifyError, unifyMany) where

import Control.Monad (unless)
import qualified Data.Map as M
import TypeInference.Subst
import TypeInference.Type

-- 型の一致に失敗したときのエラー
data UnifyError
  = UnifyMismatch Type Type
  | UnifyOccursCheckFailed String Type
  deriving (Show, Eq)

-- 型の統一
unify :: Type -> Type -> Either UnifyError Subst
unify (TVar a) t = bindVar a t
unify t (TVar a) = bindVar a t
unify TUnit TUnit = Right emptySubst
unify (TCon a) (TCon b)
  | a == b = Right emptySubst
  | otherwise = Left (UnifyMismatch (TCon a) (TCon b))
unify (TArrow a1 b1) (TArrow a2 b2) = do
  s1 <- unify a1 a2
  s2 <- unify (apply s1 b1) (apply s1 b2)
  Right (s2 `composeSubst` s1)
unify (TTuple xs) (TTuple ys)
  | length xs == length ys = unifyMany xs ys
  | otherwise = Left (UnifyMismatch (TTuple xs) (TTuple ys))
unify (TList a) (TList b) = unify a b
unify (TApp f1 a1) (TApp f2 a2) = do
  s1 <- unify f1 f2
  s2 <- unify (apply s1 a1) (apply s1 a2)
  Right (s2 `composeSubst` s1)
unify (TConstraint cs1 t1) (TConstraint cs2 t2)
  | length cs1 == length cs2 = do
      s1 <- unifyConstraints cs1 cs2
      s2 <- unify (apply s1 t1) (apply s1 t2)
      Right (s2 `composeSubst` s1)
  | otherwise = Left (UnifyMismatch (TConstraint cs1 t1) (TConstraint cs2 t2))
unify (TForall vs1 t1) (TForall vs2 t2)
  | vs1 == vs2 = unify t1 t2
  | otherwise = Left (UnifyMismatch (TForall vs1 t1) (TForall vs2 t2))
unify (TRecord f1) (TRecord f2)
  | M.keys f1 == M.keys f2 = unifyMany (M.elems f1) (M.elems f2)
  | otherwise = Left (UnifyMismatch (TRecord f1) (TRecord f2))
unify t1 t2 = Left (UnifyMismatch t1 t2)

-- 型変数と型を結びつける（occurs check あり）
bindVar :: String -> Type -> Either UnifyError Subst
bindVar v t
  | t == TVar v = Right emptySubst
  | occursIn v t = Left (UnifyOccursCheckFailed v t)
  | otherwise = Right (singletonSubst v t)

-- 型変数 v が型 t の中に現れるか（無限型を防ぐ）
occursIn :: String -> Type -> Bool
occursIn v t = case t of
  TVar x -> x == v
  TCon _ -> False
  TUnit -> False
  TList t1 -> occursIn v t1
  TTuple ts -> any (occursIn v) ts
  TArrow t1 t2 -> occursIn v t1 || occursIn v t2
  TApp t1 t2 -> occursIn v t1 || occursIn v t2
  TConstraint cs t1 -> any (occursInConstraint v) cs || occursIn v t1
  TForall vs t1 -> if v `elem` vs then False else occursIn v t1
  TRecord fields -> any (occursIn v) (M.elems fields)

-- 制約内に変数が現れるか
occursInConstraint :: String -> Constraint -> Bool
occursInConstraint v (Constraint _ ts) =
  any (occursIn v) ts

-- 制約の統一（単純な構造の一致を仮定）
unifyConstraints :: [Constraint] -> [Constraint] -> Either UnifyError Subst
unifyConstraints [] [] = Right emptySubst
unifyConstraints (Constraint c1 ts1 : cs1) (Constraint c2 ts2 : cs2)
  | c1 == c2 && length ts1 == length ts2 = do
      s1 <- unifyMany ts1 ts2
      s2 <- unifyConstraints (map (applyConstraint s1) cs1) (map (applyConstraint s1) cs2)
      Right (s2 `composeSubst` s1)
  | otherwise = Left (UnifyMismatch (TCon c1) (TCon c2))
unifyConstraints _ _ = Left (UnifyMismatch (TCon "constraint") (TCon "constraint"))

-- 型のリストを順に統一
unifyMany :: [Type] -> [Type] -> Either UnifyError Subst
unifyMany [] [] = Right emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (map (apply s1) ts1) (map (apply s1) ts2)
  Right (s2 `composeSubst` s1)
unifyMany ts1 ts2 =
  Left (UnifyMismatch (TTuple ts1) (TTuple ts2))
