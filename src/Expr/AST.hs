module Expr.AST where

data Pattern
  = PVar String
  | PInt Int
  | PWildcard
  | PCons Pattern Pattern
  | PList [Pattern]
  | PTuple [Pattern]
  | PConstr String [Pattern]
  | PApp Pattern Pattern -- ← これを追加！
  | PAs String Pattern -- ← 追加！
  deriving (Show, Eq)

data Expr
  = EVar String
  | EInt Int
  | EBinOp String Expr Expr
  | ELet [(Pattern, Expr)] Expr
  | EIf Expr Expr Expr
  | ELam String Expr
  | EApp Expr Expr
  | -- | ECase Expr [(Pattern, Expr)]
    ECase Expr [CaseAlt]
  | EList [Expr]
  | ETuple [Expr]
  | ERange Expr Expr
  | EListComp Expr [Qualifier]
  | EAnn Expr Type
  | EDo [Stmt]
  | ESeq [Expr] -- ← 追加！
  | EReturn Expr
  | ERecord [(String, Expr)]
  | ERecordUpdate Expr [(String, Expr)]
  | EOpSectionL String Expr
  | EOpSectionR Expr String
  deriving (Eq, Show)

data Stmt
  = Bind Pattern Expr
  | ExprStmt Expr
  | LetStmt [(Pattern, Expr)] -- ← 追加！
  deriving (Show, Eq)

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

data CaseAlt
  = CaseAlt Pattern Expr -- ガードなし
  | CaseAltGuard Pattern [(Expr, Expr)] -- ガード付き
  deriving (Eq, Show)
