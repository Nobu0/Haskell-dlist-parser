{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ListParserCore (listExprCore) where

import AST.Expr
import Control.Applicative (many, optional, (<|>))
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore -- (exprCore)
import Parser.Expr.PatternParser (pattern)
import Utils.MyTrace

listExprCore :: Parser Expr -> Parser Expr
listExprCore expr =
  do
    skipNL
    brackets $
      try (listCompExpr expr) -- ★ 最優先
        <|> try (stepRangeExpr expr)
        <|> try (rangeExpr expr)
        <|> listLiteralExpr expr

-- [a, b .. c]
stepRangeExpr :: Parser Expr -> Parser Expr
stepRangeExpr expr = do
  start <- expr
  symbol ","
  step <- expr
  symbol ".."
  end <- optional (try expr)
  return (ERangeStep start step end)

-- [a .. b]
rangeExpr :: Parser Expr -> Parser Expr
rangeExpr expr = do
  start <- expr
  symbol ".."
  end <- optional (try expr)
  return (ERange start end)

-- [a, b, c]
listLiteralExpr :: Parser Expr -> Parser Expr
listLiteralExpr expr = do
  elems <- many1 (elmsExpr expr)
  return (EList elems)

elmsExpr :: Parser Expr -> Parser Expr
elmsExpr expr = do
  e <- expr
  ct <- getRemainingCount
  myTrace ("<< elmsEXpr ct=" ++ show ct ++ " e " ++ show e)
  optional (symbol ",")
  skipNL
  -- optional (symbol "," <|> symbol ";")
  return e

-- [x | ...]
listCompExpr :: Parser Expr -> Parser Expr
listCompExpr expr = do
  body <- expr
  symbol "|"
  qs <- sepBy1 (qualifier expr) (symbol ",")
  return (EListComp body qs)

qualifier :: Parser Expr -> Parser Qualifier
qualifier expr =
  try (letQualifier expr)
    <|> try (genQualifier expr)
    <|> guardQualifier expr

genQualifier :: Parser Expr -> Parser Qualifier
genQualifier expr = do
  pat <- pattern
  symbol "<-"
  -- skipNL
  src <- expr
  return (QGenerator pat src)

guardQualifier :: Parser Expr -> Parser Qualifier
guardQualifier expr = QGuard <$> expr

letQualifier :: Parser Expr -> Parser Qualifier
letQualifier expr = do
  keyword "let"
  binds <- sepBy1 binding (symbol ",")
  return (QLet binds)
  where
    binding = do
      skipNL
      pat <- pattern
      symbol "="
      -- skipNL
      e <- expr
      return (pat, e)
