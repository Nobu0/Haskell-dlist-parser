module Language.TypeSystem.Pattern where

import Language.TypeSystem.BaseType

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
  | PRecord [(Name, Pattern)]
  deriving (Show, Eq)

{-}
data Pattern
  = PVar Name
  | PWildcard
  | PAs Name Pattern
  | PLit Literal
  | PTuple [Pattern]
  | PList [Pattern]
  | PCons Pattern Pattern
  | PConstr Name [Pattern]
  | PUnit
  deriving (Eq, Show)
-}