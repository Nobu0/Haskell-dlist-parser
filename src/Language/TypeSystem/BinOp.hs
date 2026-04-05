module Language.TypeSystem.BinOp (BinOp (..), binOpName) where

import Language.TypeSystem.BaseType

-- import Language.TypeSystem.Expr
{-}
data BinOp
  = BinOpAdd
  | BinOpSub
  | BinOpMul
  | BinOpDiv
  | BinOpEq
  | BinOpNeq
  | BinOpLt
  | BinOpGt
  | BinOpLe
  | BinOpGe
  | BinOpAnd
  | BinOpOr
  | BinOpConcat
  | BinOpCons
  | BinOpCompose
  | BinOpThen
  | BinOpBind
  | BinOpAlt
  | BinOpFmap
  | BinOpMap
  | BinOpApp
  | BinOpAppend
  | BinOpListDiff
  | BinOpApplyL
  | BinOpApplyR
  deriving (Show, Ord, Eq)
-}

-- | 二項演算子の列挙型
data BinOp
  = Add -- +
  | Sub -- -
  | Mul
  | Div -- /
  | Eq -- ==
  | Neq -- /=
  | Lt -- <
  | Gt -- >
  | Le -- <=
  | Ge -- >=
  | And -- "&&"
  | Or -- "||"
  | Concat -- "++"
  | Cons -- ":"
  | Compose -- "."
  | Then -- ">>"
  | Bind -- ">>="
  | Alt -- "<|"
  | Fmap -- "<$>"
  | App -- "<*>"
  deriving (Eq, Ord, Show)

-- | BinOp を文字列（演算子名）に変換
binOpName :: BinOp -> Name
binOpName op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Eq -> "=="
  Neq -> "/="
  Lt -> "<"
  Gt -> ">"
  Le -> "<="
  Ge -> ">="
  And -> "&&"
  Or -> "||"
  Concat -> "++"
  Cons -> ":"
  Compose -> "."
  Then -> ">>"
  Bind -> ">>="
  Alt -> "<|>"
  Fmap -> "<$>"
  App -> "<*>"
