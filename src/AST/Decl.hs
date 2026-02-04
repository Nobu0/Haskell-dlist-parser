module AST.Decl where

import AST.Pattern (Pattern)
import AST.Expr
import AST.Type (Type)
-- import AST.Module (Name)

-- type Binding = (Pattern, Expr)

data Decl
  = DeclFun Pattern Expr
  | DeclValue Pattern Expr
  | DeclData Name [Name] [Constr]
  | DeclNewtype Name [Name] Constr
  | DeclImport Name
  | DeclModule Name
  deriving (Show, Eq)

data Constr = Constr Name [Type]
  deriving (Show, Eq)
