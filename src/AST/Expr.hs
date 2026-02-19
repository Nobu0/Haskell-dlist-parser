module AST.Expr where

import AST.Pattern (Pattern)
import AST.Type (Type)

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
  | QLet [(Pattern, Expr)]
  | QGuard Expr
  deriving (Eq, Show)

-- Expr 本体
data Expr
  = EVar String
  | EVarType String
  | EInt Int
  | EString String
  | EChar Char
  | EBinOp BinOp Expr Expr
  | EBool Bool
  | ELet Pattern Expr Expr
  | ELetBlock [(Pattern, Expr)] Expr
  | EIf Expr Expr Expr
  | ELam Pattern Expr
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
  | EUnit
  | ERecord [(String, Expr)]
  | ERecordUpdate Expr [(String, Expr)]
  | EOpSectionL String Expr
  | EOpSectionR Expr String
  | EPlaceholder
  | EWhere Expr [Binding]
  | ELambdaCase [CaseAlt] -- [(Pattern, Expr)]
  | ESQL String [Expr]
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | Concat
  | Cons
  deriving (Eq, Show)
