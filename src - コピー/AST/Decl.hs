module AST.Decl where

import AST.Expr
import AST.Pattern (Pattern)
import AST.Type (Type)

data Decl
  = DeclFun Name [Pattern] Expr
  | DeclValue Pattern Expr
  | DeclTypeSig Name Type
  | DeclData Name [Name] [Constr]
  | DeclNewtype Name [Name] Constr
  | DeclImport Name
  | DeclModule Name
  deriving (Show, Eq)

data Constr = Constr Name [Type]
  deriving (Show, Eq)
