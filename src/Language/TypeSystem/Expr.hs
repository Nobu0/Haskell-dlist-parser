module Language.TypeSystem.Expr where

import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.BinOp (BinOp (..))
import Language.TypeSystem.Pattern (Pattern)
import Language.TypeSystem.Syntax

-- | 式（推論対象のAST）
data Expr
  = EVar Name
  | EInt Int
  | EBool Bool
  | EChar Char
  | EString String
  | EUnit
  | EBinOp BinOp Expr Expr
  | ELam [Pattern] Expr
  | EApp Expr Expr
  | ELet Pattern Expr Expr
  | ELetBlock [(Pattern, Expr)] Expr
  | EIf Expr Expr Expr
  | ECase Expr [CaseAlt]
  | ETuple [Expr]
  | EList [Expr]
  | EAnn Expr Type
  | ERecord (Map.Map String Expr)
  | EFieldAccess Expr String
  | ERecordUpdate Expr [(String, Expr)]
  | EOpSectionL BinOp Expr
  | EOpSectionR Expr BinOp
  | EWhere Expr [Binding]
  | ELambdaCase [CaseAlt]
  deriving (Eq, Show)

-- | パターンと式のペア（letやwhereで使う）
type Binding = (Pattern, Expr)

-- | case式の分岐
data CaseAlt
  = CaseAlt Pattern Expr
  | CaseAltGuard Pattern [(Expr, Expr)] -- ガード付き
  deriving (Eq, Show)
