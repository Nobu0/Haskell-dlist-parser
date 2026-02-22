module AST.Decl where

import AST.Expr
import AST.Pattern (Pattern)
import AST.Type (Constraint, Type)

-- DeclFun Name [Pattern] (Maybe [(Expr, Expr)]) (Maybe Expr) (Maybe [Decl])

data Decl
  = DeclFunGroup Name [FunClause]
  | DeclValue Pattern Expr
  | DeclTypeSig Name Type
  | DeclData Name [Name] [Constraint] [Name]
  | DeclNewtype Name [Name] Constraint
  | DeclModule String (Maybe [Export])
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

data FunClause
  = FunClause [Pattern] (Maybe [(Expr, Expr)]) (Maybe Expr) (Maybe [Decl])
  deriving (Show, Eq)

data ImportItem
  = ImportVar Name
  | ImportTypeAll Name
  | ImportTypeSome Name [Name]
  | ImportAllItems -- ← 追加！
  deriving (Show, Eq)

data Export
  = ExportVar String -- foo
  | ExportType String Bool -- Bar or Bar(..)
  deriving (Show, Eq)

-- data Constr = Constr Name [Type]
--  deriving (Show, Eq)
