module Expr.ExprExtensions (exprWithDo) where

import Expr.DoParser (doExpr)
import Expr.ExprParser (expr)

exprWithDo :: Parser Expr
exprWithDo =
  try doExpr
    <|> expr
