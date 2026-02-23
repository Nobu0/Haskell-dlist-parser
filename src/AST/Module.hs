module AST.Module where

import AST.Decl (Decl)
import AST.Type

type Name = String

data Module
  = Module
  { moduleName :: Name,
    moduleImports :: [Import],
    moduleDecls :: [Decl]
  }
  deriving (Show, Eq)

data Import
  = Import
  { importName :: Name
  }
  deriving (Show, Eq)
