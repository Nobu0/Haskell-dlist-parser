{-# LANGUAGE LambdaCase #-}

module Expr.ExprNoListParser (exprNoList) where

import Control.Applicative
-- import Text.Megaparsec (anySingle, lookAhead)

-- === 式の構文解析 ===

import Data.Functor (void)
import Expr.AST
import Expr.Combinator
import Expr.ExprParser (binOp, caseExpr, doExpr, ifExpr, lambdaExpr, letExpr)
-- import Expr.ListParser
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..))
import MyTrace (myTrace, myTraceIO, myTraceShowId)

atomBaseNoList :: Parser Expr
atomBaseNoList = do
  t <- lookAhead anyToken
  myTrace ("<< atomBaseNoList next token: " ++ show t)
  try letExpr
    <|> (token TokEllipsis >> return EPlaceholder)
    <|> EVar
      <$> ident
    <|> EInt
      <$> int
    <|> pRecordExpr
    <|> tupleOrParenNoList

tupleOrParenNoList :: Parser Expr
tupleOrParenNoList = do
  symbol "("
  e1 <- exprNoList
  ( do
      symbol ","
      es <- sepBy1 exprNoList (symbol ",")
      symbol ")"
      return (ETuple (e1 : es))
    )
    <|> (symbol ")" >> return e1)

appExprNoList :: Parser Expr
appExprNoList = chainl1 atomBaseNoList (return EApp)

exprLevel1NoList :: Parser Expr
exprLevel1NoList = chainl1 exprLevel2NoList (binOp ["+", "-"])

exprLevel2NoList :: Parser Expr
exprLevel2NoList = chainl1 exprLevel3NoList (binOp ["*", "/"])

exprLevel3NoList :: Parser Expr
exprLevel3NoList =
  try forExpr
    <|> try returnExpr
    <|> try doExpr
    <|> try ifExpr
    <|> try lambdaExpr
    <|> appExprNoList

exprCmpNoList :: Parser Expr
exprCmpNoList = chainl1 exprLevel1NoList (binOp [">", "<", ">=", "<=", "==", "/="])

binOpExprNoList :: Parser Expr
binOpExprNoList = exprCmpNoList

exprNoList :: Parser Expr
exprNoList = do
  t <- lookAhead anyToken
  myTrace ("<< exprNoList next token: " ++ show t)
  try caseExpr
    <|> try doExpr
    <|> try ifExpr
    <|> try lambdaExpr
    <|> try binOpExprNoList
    <|> exprLevel1NoList
