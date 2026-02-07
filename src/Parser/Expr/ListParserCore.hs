{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ListParserCore (listExprCore) where

import AST.Expr
import Control.Applicative (many, optional, (<|>))
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser (brackets, keyword, symbol)
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.PatternParser (pattern)
import Utils.MyTrace

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

{-}
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
-}

letQualifier :: Parser Expr -> Parser Qualifier
letQualifier expr = do
  keyword "let"
  binds <- sepBy1 binding (symbol ",")
  return (QLet binds)
  where
    binding = do
      pat <- pattern
      symbol "="
      e <- expr
      return (pat, e)
