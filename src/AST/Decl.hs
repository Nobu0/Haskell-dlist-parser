module AST.Decl where

import AST.Expr
import AST.Pattern (Pattern)
import AST.Type (Constraint, Type)

data Decl
  = DeclFun Name [Pattern] Expr
  | DeclValue Pattern Expr
  | DeclTypeSig Name Type
  | DeclData Name [Name] [Constraint]
  | DeclNewtype Name [Name] Constraint
  | -- | DeclImport Name (Maybe [Name])
    DeclModule Name
  | DeclClass String [String] [Decl]
  | DeclInstance (Maybe [Constraint]) String [Type] [Decl]
  | DeclTypeAlias String [String] Type
  | DeclImport
      { importQualified :: Bool,
        importModule :: Name,
        importAlias :: Maybe Name,
        importHiding :: Bool,
        importItems :: Maybe [ImportItem]
      }
  deriving (Show, Eq)

data ImportItem
  = ImportVar Name
  | ImportTypeAll Name
  | ImportTypeSome Name [Name]
  | ImportAllItems -- ← 追加！
  deriving (Show, Eq)

-- data Constr = Constr Name [Type]
--  deriving (Show, Eq)
