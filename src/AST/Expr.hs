module AST.Expr where

import AST.Type (Type)
import AST.Pattern (Pattern)

type Name = String

-- Binding
type Binding = (Pattern, Expr)

-- CaseAlt
data CaseAlt
  = CaseAlt Pattern Expr
  | CaseAltGuard Pattern [(Expr, Expr)]
  deriving (Eq, Show)

-- Stmt
data Stmt
  = Bind Pattern Expr
  | ExprStmt Expr
  | LetStmt [(Pattern, Expr)]
  deriving (Eq, Show)

-- Qualifier
data Qualifier
  = QGenerator Pattern Expr
  | QGuard Expr
  deriving (Eq, Show)

-- Expr 本体
data Expr
  = EVar String
  | EVarType String
  | EInt Int
  | EString String
  | EBinOp String Expr Expr
  | ELet [Binding] Expr
  | EIf Expr Expr Expr
  | ELam String Expr
  | EApp Expr Expr
  | ECase Expr [CaseAlt]
  | EList [Expr]
  | ETuple [Expr]
  | ERange Expr Expr
  | ERangeStep Expr Expr Expr
  | EListComp Expr [Qualifier]
  | EAnn Expr Type
  | EDo [Stmt]
  | ESeq [Expr]
  | EReturn Expr
  | ERecord [(String, Expr)]
  | ERecordUpdate Expr [(String, Expr)]
  | EOpSectionL String Expr
  | EOpSectionR Expr String
  | EPlaceholder
  | EWhere Expr [Binding]
  deriving (Eq, Show)
