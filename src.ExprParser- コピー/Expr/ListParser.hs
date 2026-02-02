{-# LANGUAGE LambdaCase #-}

module Expr.ListParser
  ( listExpr,
  )
where

import Control.Applicative
-- import Text.Megaparsec (anySingle, lookAhead)

-- === 式の構文解析 ===

import Data.Functor (void)
import Expr.AST
import Expr.Combinator
import Expr.ExprNoListParser (exprNoList)
import Expr.ExprParser (expr)
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..))
import MyTrace (myTrace, myTraceIO, myTraceShowId)

listExpr :: Parser Expr
listExpr =
  brackets
    ( try listComprehension
        <|> try rangeStep
        <|> try range
        <|> normalList
    )

listItemExpr :: Parser Expr
listItemExpr = exprNoList

rangeStep :: Parser Expr
rangeStep = try $ do
  start <- listItemExpr
  symbol ","
  step <- listItemExpr
  symbol ".."
  end <- listItemExpr
  return (ERangeStep start step end)

range :: Parser Expr
range = try $ do
  start <- listItemExpr
  symbol ".."
  end <- listItemExpr
  return (ERange start end)

listLiteral :: Parser Expr
listLiteral = do
  elems <- sepBy listItemExpr (symbol ",")
  symbol "]"
  return (EList elems)

listComprehension :: Parser Expr
listComprehension = do
  body <- listItemExpr
  symbol "|"
  qs <- sepBy1 qualifier (symbol ",")
  -- symbol "]"
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  symbol "<-"
  e <- listItemExpr
  return (QGenerator pat e)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> listItemExpr

normalList :: Parser Expr
-- normalList = EList <$> sepBy expr (symbol ",")
normalList = EList <$> sepEndBy listItemExpr (symbol ",")

listComp :: Parser Expr
listComp = do
  body <- listItemExpr
  symbol "|"
  qualifiers <- qualifier `sepBy` symbol ","
  return (EListComp body qualifiers)
