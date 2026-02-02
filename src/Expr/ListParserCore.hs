{-# LANGUAGE LambdaCase #-}

module Expr.ListParserCore (listExprCore) where

import Control.Applicative (many, optional, (<|>))
import Expr.AST
import Expr.Combinator
import Expr.ExprCore (exprCore)
import Expr.PatternParser
import Expr.TokenParser

{-}
listExprCore :: Parser Expr -> Parser Expr
listExprCore expr =
  brackets $
    try (stepRangeExpr expr)
      <|> try (rangeExpr expr)
      <|> try (listLiteralExpr expr)
      <|> listCompExpr expr
-}

listExprCore :: Parser Expr -> Parser Expr
listExprCore expr =
  brackets $
    try (listCompExpr expr) -- ★ 最優先
      <|> try (stepRangeExpr expr)
      <|> try (rangeExpr expr)
      <|> listLiteralExpr expr

-- [a, b .. c]
stepRangeExpr :: Parser Expr -> Parser Expr
stepRangeExpr expr = do
  start <- exprCore
  symbol ","
  step <- exprCore
  symbol ".."
  end <- exprCore
  return (ERangeStep start step end)

-- [a .. b]
rangeExpr :: Parser Expr -> Parser Expr
rangeExpr expr = do
  start <- exprCore
  symbol ".."
  end <- exprCore
  return (ERange start end)

-- [a, b, c]
listLiteralExpr :: Parser Expr -> Parser Expr
listLiteralExpr expr = do
  elems <- sepBy exprCore (symbol ",")
  optional (symbol ",")
  return (EList elems)

-- [x | ...]
listCompExpr :: Parser Expr -> Parser Expr
listCompExpr expr = do
  body <- exprCore
  symbol "|"
  qs <- sepBy1 (qualifier expr) (symbol ",")
  return (EListComp body qs)

qualifier :: Parser Expr -> Parser Qualifier
qualifier expr =
  try (genQualifier expr) -- ★ 最優先
    <|> try (letQualifier expr)
    <|> guardQualifier expr

genQualifier :: Parser Expr -> Parser Qualifier
genQualifier expr = do
  pat <- pattern
  symbol "<-"
  src <- expr
  return (QGenerator pat src)

guardQualifier :: Parser Expr -> Parser Qualifier
guardQualifier expr = QGuard <$> expr

letQualifier :: Parser Expr -> Parser Qualifier
letQualifier expr = do
  keyword "let"
  binds <- sepBy1 binding (symbol ",")
  return (QGuard (ELet binds (EVar "__unit__")))
  where
    binding = do
      pat <- pattern
      symbol "="
      e <- expr
      return (pat, e)

{-}
listExpr :: Parser Expr
listExpr =
  brackets $
    try stepRangeExpr
      <|> try rangeExpr
      <|> try listLiteralExpr
      <|> listCompExpr

listLiteralExpr :: Parser Expr
listLiteralExpr = do
  elems <- sepBy exprCore (symbol ",")
  optional (symbol ",") -- 許容: [1,2,3,]
  return (EList elems)

stepRangeExpr :: Parser Expr
stepRangeExpr = do
  start <- exprCore
  symbol ","
  step <- exprCore
  symbol ".."
  end <- exprCore
  return (ERangeStep start step end)

rangeExpr :: Parser Expr
rangeExpr = do
  start <- exprCore
  symbol ".."
  end <- exprCore
  return (ERange start end)

listCompExpr :: Parser Expr
listCompExpr = do
  body <- exprCore
  qs <- many qualifier
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  keyword "in"
  src <- exprCore
  return (QGenerator pat src)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> exprCore
-}
