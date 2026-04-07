{-# LANGUAGE TypeSynonymInstances #-}

module Language.TypeSystem.Subst
  ( Subst,
    emptySubst,
    nullSubst,
    composeSubst,
    composeMany,
    applySubst,
    singletonSubst,
    -- freeTypeVars,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.BaseType
-- (Types (..))

import Language.TypeSystem.Class (Types (..))
import Language.TypeSystem.ClassDef
import Language.TypeSystem.Env
import Language.TypeSystem.Syntax
import Language.TypeSystem.Syntax (Type (..))

-- freeTypeVars :: (Types a) => a -> Set.Set Name
-- freeTypeVars = ftv

-- | 置換：型変数名から型へのマップ
-- type Subst = Map.Map Name Type
-- | 空の置換
emptySubst :: Subst
emptySubst = Map.empty

-- | 空の置換
nullSubst :: Subst
nullSubst = Map.empty

singletonSubst :: Name -> Type -> Subst
singletonSubst v t = Map.singleton v t

-- | 置換の合成（右側を先に適用）
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1

composeMany :: [Subst] -> Subst
composeMany = foldr composeSubst nullSubst
