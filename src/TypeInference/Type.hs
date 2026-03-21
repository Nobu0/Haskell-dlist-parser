module TypeInference.Type where

import AST.BinOp (BinOp (..))
import qualified Data.Map as Map

-- 型の名前（型変数）を表す型
type Name = String

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
  | TBinOp BinOp Type Type
  | TRecord (Map.Map String Type)
  deriving (Eq, Show)

data Constraint = Constraint String [Type]
  deriving (Eq, Show)
