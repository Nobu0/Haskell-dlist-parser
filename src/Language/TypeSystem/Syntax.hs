{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.TypeSystem.Syntax where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.BaseType
import Language.TypeSystem.BinOp

-- | 型変数（例: "a", "b", "t1" など）
type TVar = String

-- | 型名（例: "Int", "Bool", "List" など）
type TyCon = String

-- | 識別子（変数や関数の名前）
-- type Name = String

-- | 型の定義

{-}
data Type
  = TVar TVar
  | TCon TyCon
  | TArrow Type Type
  | TTuple [Type]
  | TList Type
  deriving (Eq, Ord, Show)
-}

data Type
  = TVar String
  | TCon String
  | TArrow Type Type
  | TList Type
  | TApp Type Type
  | TConstraint [Constraint] Type
  | TForall [String] Type
  | TTuple [Type]
  | TUnit
  | TFun Type Type
  | TBinOp BinOp Type Type
  | TRecord (Map.Map String Type) (Maybe Type) -- フィールド + 行変数
  deriving (Eq, Ord, Show)

-- data BinOp = Add | Mul | Sub | And | Or
--  deriving (Eq, Ord, Show)

-- | 型クラス制約（述語）
data Pred
  = IsIn String Type -- 例: IsIn "Eq" (TVar "a") ≡ Eq a
  deriving (Eq, Ord, Show)

-- | 型スキーム（量化された型 + 制約）
data Scheme = Forall [TVar] [Pred] Type
  deriving (Eq, Ord, Show)

-- | 型の置換（型変数 → 型）
type Subst = Map.Map TVar Type

-- | 型環境（名前 → 型スキーム）
-- type TypeEnv = Map.Map Name Scheme
-- newtype TypeEnv = TypeEnv (Map.Map Name Scheme)
--  deriving (Eq, Show)

-- | 制約（型の等式制約）
type Constraint = (Type, Type)
