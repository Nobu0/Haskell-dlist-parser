module TypeInference.Unify (Subst, unify, UnifyError) where

import AST.Type
import Control.Monad (unless)
import qualified Data.Map as M
import TypeInference.Subst

-- 型の一致に失敗したときのエラー
data UnifyError
  = UnifyMismatch Type Type
  | UnifyOccursCheckFailed String Type
  deriving (Show, Eq)

-- 型の一致を試み、成功すれば代入を返す
unify :: Type -> Type -> Either UnifyError Subst
unify (TArrow l1 r1) (TArrow l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  return (composeSubst s2 s1)
unify (TApp f1 a1) (TApp f2 a2) = do
  s1 <- unify f1 f2
  s2 <- unify (apply s1 a1) (apply s1 a2)
  return (composeSubst s2 s1)
unify (TList t1) (TList t2) =
  unify t1 t2
unify (TCon c1) (TCon c2)
  | c1 == c2 = return emptySubst
  | otherwise = Left (UnifyMismatch (TCon c1) (TCon c2))
unify (TVar v) t = bindVar v t
unify t (TVar v) = bindVar v t
-- その他の型構成子は未対応（TConstraint, TForall など）
unify t1 t2 = Left (UnifyMismatch t1 t2)

-- 型変数と型を結びつける（occurs check あり）
bindVar :: String -> Type -> Either UnifyError Subst
bindVar v t
  | t == TVar v = return emptySubst
  | v `occursIn` t = Left (UnifyOccursCheckFailed v t)
  | otherwise = return (singletonSubst v t)

-- 型変数 v が型 t の中に現れるか（無限型を防ぐ）
occursIn :: String -> Type -> Bool
occursIn v t = case t of
  TVar x -> x == v
  TCon _ -> False
  TArrow t1 t2 -> occursIn v t1 || occursIn v t2
  TList t1 -> occursIn v t1
  TApp t1 t2 -> occursIn v t1 || occursIn v t2
  TConstraint cs t1 -> any (occursInConstraint v) cs || occursIn v t1
  TForall vs t1 -> if v `elem` vs then False else occursIn v t1

occursInConstraint :: String -> Constraint -> Bool
occursInConstraint v (Constraint _ ts) =
  any (occursIn v) ts
