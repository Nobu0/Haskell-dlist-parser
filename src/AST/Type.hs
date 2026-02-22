module AST.Type where

import AST.BinOp

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
  deriving (Eq, Show)

data Constraint
  = Constraint String [Type] -- 通常のコンストラクタ
  | ConstraintRecord String [Field] -- レコード構文のコンストラクタ
  deriving (Show, Eq)

data Field
  = Field String Type
  deriving (Show, Eq)

-- data Constraint = Constraint String [Type]
--  deriving (Eq, Show)
