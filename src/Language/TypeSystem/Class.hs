{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.TypeSystem.Class
  ( Types (..),
    SchemeLike (..),
    EnvLike (..),
    ConstraintLike (..),
    freeTypeVars,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.BaseType
import Language.TypeSystem.ClassDef
import Language.TypeSystem.Env
import Language.TypeSystem.InferM
import Language.TypeSystem.Syntax

freeTypeVars :: (Types a) => a -> Set.Set Name
freeTypeVars = ftv

-- | Type への適用
instance Types Type where
  applySubst s t = case t of
    TVar n -> Map.findWithDefault t n s
    TCon _ -> t
    TApp l r -> TApp (applySubst s l) (applySubst s r)
    TArrow a b -> TArrow (applySubst s a) (applySubst s b)
    TTuple ts -> TTuple (map (applySubst s) ts)
    TList t1 -> TList (applySubst s t1)

  ftv t = case t of
    TVar n -> Set.singleton n
    TCon _ -> Set.empty
    TApp l r -> ftv l `Set.union` ftv r
    TArrow a b -> ftv a `Set.union` ftv b
    TTuple ts -> Set.unions (map ftv ts)
    TList t1 -> ftv t1

-- | Scheme への適用（束縛変数は無視）
instance Types Scheme where
  applySubst s (Forall vars preds t) =
    let s' = foldr Map.delete s vars
     in Forall vars (map (applySubst s') preds) (applySubst s' t)

  ftv (Forall vars preds t) =
    (ftv preds `Set.union` ftv t) `Set.difference` Set.fromList vars

-- | Pred への適用
instance Types Pred where
  applySubst s (IsIn cls t) = IsIn cls (applySubst s t)
  ftv (IsIn _ t) = ftv t

-- | リストへの適用
instance (Types a) => Types [a] where
  applySubst s = map (applySubst s)
  ftv = foldr (Set.union . ftv) Set.empty

-- | 型環境への適用
instance Types TypeEnv where
  applySubst s (TypeEnv env) = TypeEnv (Map.map (applySubst s) env)
  ftv (TypeEnv env) = ftv (Map.elems env)
