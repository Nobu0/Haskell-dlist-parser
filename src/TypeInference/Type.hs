module TypeInference.Type where

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
  deriving (Eq, Show)

-- Constraint の定義がまだ無い場合は仮で置く
data Constraint = Constraint String
  deriving (Eq, Show)
