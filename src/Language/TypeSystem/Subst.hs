{-# LANGUAGE TypeSynonymInstances #-}

module Language.TypeSystem.Subst
  ( Subst,
    emptySubst,
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

{-}
-- | 置換を適用できる型クラス
class Types a where
  applySubst :: Subst -> a -> a
  freeTypeVars :: a -> Set.Set Name
-}
-- ftv :: a -> Set.Set Name
{-}
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
-}

{-}
instance Types TypeEnv where
  applySubst s env = Map.map (applySubst s) env
  ftv env = ftv (Map.elems env)
-}
{-}
module Language.TypeSystem.Subst where

import qualified Data.Map as Map
import Language.TypeSystem.Class
import Language.TypeSystem.Syntax

-- | 単一の置換を作る
singletonSubst :: TVar -> Type -> Subst
singletonSubst v t = Map.singleton v t

-- | 置換の合成（s1 を先に適用し、その後 s2 を適用）
-- composeSubst s2 s1 ≡ s2 ∘ s1
composeSubst :: Subst -> Subst -> Subst
composeSubst s2 s1 =
  Map.map (applySubst s2) s1 `Map.union` s2

-- | 複数の置換をまとめて合成（右から左へ）
composeMany :: [Subst] -> Subst
composeMany = foldr composeSubst emptySubst

-}
