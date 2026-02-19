module AST.Pattern where

-- import AST.Type (Type)
-- import AST.Module (Name)
-- import AST.Pattern (Pattern)
-- import AST.Expr (Expr)

type Name = String

data Pattern
  = PVar Name
  | PInt Int
  | PChar Char
  | PString String
  | PWildcard
  | PCons Pattern Pattern
  | PList [Pattern]
  | PTuple [Pattern]
  | PConstr Name [Pattern]
  | PAs Name Pattern -- ← 追加！
  | PApp Pattern [Pattern] -- ★ 修正
  | PInfix Pattern Name Pattern
  deriving (Show, Eq)
