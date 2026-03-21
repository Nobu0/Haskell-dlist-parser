module AST.Pattern where

-- type Name = String
import AST.Type (Name)

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
