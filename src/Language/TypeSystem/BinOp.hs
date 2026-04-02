module Language.TypeSystem.BinOp where

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
