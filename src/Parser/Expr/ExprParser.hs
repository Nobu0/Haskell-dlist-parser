{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ExprParser
  ( exprCore,
    exprTop,
    exprSeq,
    exprSep,
  )
where

import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.ExprExtensions (exprSep, exprSeq, exprTop)