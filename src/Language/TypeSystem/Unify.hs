module Language.TypeSystem.Unify where

import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.Class -- (TypeLike (..))
import Language.TypeSystem.Error
import Language.TypeSystem.InferM
import Language.TypeSystem.Subst -- (composeSubst, emptySubst)
import Language.TypeSystem.Syntax

-- | 複数の型を順に統一する
unifyMany :: [Type] -> [Type] -> InferM Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (map (applySubst s1) ts1) (map (applySubst s1) ts2)
  return (s2 `composeSubst` s1)
unifyMany t1 t2 =
  throwError $ UnificationMismatch (TTuple t1) (TTuple t2)

-- | 型変数に型を束縛する（ただし occurs check を行う）
bindVar :: TVar -> Type -> InferM Subst
bindVar v t
  | t == TVar v = return emptySubst
  | v `Set.member` freeTypeVars t = throwError $ InfiniteType v t
  | otherwise = return $ singletonSubst v t

-- | 型の統一（t1 と t2 を等しくするための置換を返す）
unify :: Type -> Type -> InferM Subst
unify (TArrow l1 r1) (TArrow l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  return (s2 `composeSubst` s1)
unify (TList t1) (TList t2) =
  unify t1 t2
unify (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyMany ts1 ts2
  | otherwise = throwError $ UnificationFail (TTuple ts1) (TTuple ts2)
unify (TApp f1 x1) (TApp f2 x2) = do
  s1 <- unify f1 f2
  s2 <- unify (applySubst s1 x1) (applySubst s1 x2)
  return (s2 `composeSubst` s1)
unify (TVar u) t = bindVar u t
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
unify (TRecord f1) (TRecord f2)
  | Map.keysSet f1 == Map.keysSet f2 = unifyMany (Map.elems f1) (Map.elems f2)
  | otherwise = throwError $ UnificationMismatch (TRecord f1) (TRecord f2)
unify TUnit TUnit = return emptySubst
unify t1 t2 = throwError $ UnificationMismatch t1 t2
