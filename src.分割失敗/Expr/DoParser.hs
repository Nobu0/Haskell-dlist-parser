module Expr.DoParser (doExpr, doSemi) where

import Control.Applicative (many, optional, (<|>))
import Expr.AST
import Expr.Combinator
-- import Expr.ExprCore (exprCore)
-- ★ これが必要

import Expr.ExprExtensions (expr, returnExpr)
-- import Expr.Parser (expr)
import Expr.PatternParser
import Expr.TokenParser
import Lexer (Token (..))
import MyTrace

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
doBlock = sepBy doStmt doSemi

doStmt :: Parser Stmt
doStmt =
  try bindStmt
    <|> try letStmt
    -- <|> try returnExpr -- returnStmt
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

{-}
returnStmt :: Parser Stmt
returnStmt = do
  keyword "return"
  e <- expr
  return (Return e)
-}

doSemi :: Parser ()
doSemi = do
  optional (token TokNewline)
  symbol ";"
