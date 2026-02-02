{-# LANGUAGE LambdaCase #-}

module Expr.DoParser
  ( doExpr,
  )
where

import Control.Applicative
-- import Text.Megaparsec (anySingle, lookAhead)

-- === 式の構文解析 ===

import Data.Functor (void)
import Expr.AST
import Expr.Combinator
import Expr.ExprParser (expr)
import Expr.ListParser (listExpr)
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..))
import MyTrace (myTrace, myTraceIO, myTraceShowId)

doStmt :: Parser Stmt
doStmt = do
  many (token TokNewline)
  try bindStmt
    <|> try letStmt
    <|> ExprStmt <$> expr

bindStmt :: Parser Stmt
bindStmt = do
  pat <- pattern
  symbol "<-"
  e <- expr
  return (Bind pat e)

letStmt :: Parser Stmt
letStmt = do
  keyword "let"
  binds <- sepBy1 binding (symbol ";")
  return (LetStmt binds)
  where
    binding = do
      pat <- pattern
      symbol "="
      e <- expr
      return (pat, e)

doExpr :: Parser Expr
doExpr = do
  keyword "do"
  symbol "{"
  many (token TokNewline)
  stmts <- doBlock
  many (token TokNewline)
  symbol "}"
  return (EDo stmts)

doBlock :: Parser [Stmt]
doBlock =
  sepBy doStmt doSemi

doSemi :: Parser ()
doSemi = do
  optional (token TokNewline)
  symbol ";"
