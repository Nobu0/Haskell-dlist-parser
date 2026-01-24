module Expr.AST where

data Pattern
  = PVar String
  | PInt Int
  | PWildcard
  | PCons Pattern Pattern
  | PList [Pattern]
  | PTuple [Pattern]
  | PConstr String [Pattern]
  deriving (Show, Eq)

data Expr
  = EVar String
  | EInt Int
  | EBinOp String Expr Expr
  | ELet [(Pattern, Expr)] Expr
  | EIf Expr Expr Expr
  | ELam String Expr
  | EApp Expr Expr
  | ECase Expr [(Pattern, Expr)]
  | EList [Expr]
  | ETuple [Expr]
  | ERange Expr Expr
  | EListComp Expr [Qualifier]
  | EAnn Expr Type
  deriving (Eq, Show)

--  | ELet [(String, Expr)] Expr

data Qualifier
  = EGenerator String Expr
  | EGuard Expr
  deriving (Eq, Show)

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
