module AST.BinOp where

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
  deriving (Show, Eq)
