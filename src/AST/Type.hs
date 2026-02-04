module AST.Type where

-- import AST.Decl (Binding)

data Type
  = TVar String
  | TCon String
  | TArrow Type Type
  | TList Type
  | TApp Type Type
  | TConstraint [Constraint] Type
  | TForall [String] Type
  deriving (Eq, Show)

data Constraint = Constraint String [Type]
  deriving (Eq, Show)
