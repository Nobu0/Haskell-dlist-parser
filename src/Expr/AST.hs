module Expr.AST where

type Name = String

type Binding = (Pattern, Expr)

data Pattern
  = PVar Name
  | PInt Int
  | PWildcard
  | PCons Pattern Pattern
  | PList [Pattern]
  | PTuple [Pattern]
  | PConstr Name [Pattern]
  | PAs Name Pattern -- ← 追加！
  | PApp Pattern [Pattern] -- ★ 修正
  deriving (Show, Eq)

data Expr
  = EVar String
  | EVarType String
  | EInt Int
  | EBinOp String Expr Expr
  | ELet [Binding] Expr
  | EIf Expr Expr Expr
  | ELam String Expr
  | EApp Expr Expr
  | -- | ECase Expr [(Pattern, Expr)]
    ECase Expr [CaseAlt]
  | EList [Expr]
  | ETuple [Expr]
  | ERange Expr Expr
  | ERangeStep Expr Expr Expr
  | EListComp Expr [Qualifier]
  | EAnn Expr Type
  | EDo [Stmt]
  | ESeq [Expr] -- ← 追加！
  | EReturn Expr
  | ERecord [(String, Expr)]
  | ERecordUpdate Expr [(String, Expr)]
  | EOpSectionL String Expr
  | EOpSectionR Expr String
  | EPlaceholder -- 追加！
  | EWhere Expr [Binding]
  deriving (Eq, Show)

{-}
data Decl
  = FunDecl Name [Pattern] Expr
  | ValueDecl Pattern Expr
-}

data Decl
  = FunDecl Name [Pattern] Expr
  | ValueDecl Pattern Expr
  deriving (Show, Eq)

{-}
data Stmt
  = BindStmt Pattern Expr
  | LetStmt [(Pattern, Expr)]
  | ExprStmt Expr
  deriving (Show, Eq)
-}
data Stmt
  = Bind Pattern Expr
  | ExprStmt Expr
  | LetStmt [(Pattern, Expr)] -- ← 追加！
  deriving (Show, Eq)

--  | ELet [(String, Expr)] Expr
{-}
data Qualifier
  = EGenerator String Expr
  | EGuard Expr
  deriving (Eq, Show)
-}

data Qualifier
  = QGenerator Pattern Expr
  | QGuard Expr
  deriving (Show, Eq)

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

{-}
data CaseAlt
  = CaseAlt Pattern Expr
  | CaseAltGuard Pattern [Expr] Expr
  deriving (Eq, Show)
-}
