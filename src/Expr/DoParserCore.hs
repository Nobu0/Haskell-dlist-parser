{-# LANGUAGE LambdaCase #-}

module Expr.DoParserCore (doExprCore) where

import Control.Applicative (many, optional, (<|>))
import Expr.AST
import Expr.Combinator
import Expr.ExprCore (exprCore)
import Expr.PatternParser
import Expr.TokenParser
import Lexer (Token (..))

doExprCore :: Parser Expr -> Parser Expr
doExprCore expr = do
  keyword "do"
  symbol "{"
  many (token TokNewline)
  stmts <- doBlock expr
  many (token TokNewline)
  symbol "}"
  return (EDo stmts)

doBlock :: Parser Expr -> Parser [Stmt]
doBlock expr = sepBy (doStmt expr) doSemi

doStmt :: Parser Expr -> Parser Stmt
doStmt expr = do
  many (token TokNewline)
  try (bindStmt expr)
    <|> try (letStmt expr)
    <|> ExprStmt <$> expr

bindStmt :: Parser Expr -> Parser Stmt
bindStmt expr = do
  pat <- pattern
  symbol "<-"
  e <- expr
  return (Bind pat e)

letStmt :: Parser Expr -> Parser Stmt
letStmt expr = do
  keyword "let"
  binds <- sepBy1 binding (symbol ";")
  return (LetStmt binds)
  where
    binding = do
      pat <- pattern
      symbol "="
      e <- expr
      return (pat, e)

doSemi :: Parser ()
doSemi = do
  optional (token TokNewline)
  symbol ";"

{-}
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
    <|> ExprStmt <$> exprCore

bindStmt :: Parser Stmt
bindStmt = do
  pat <- pattern
  symbol "<-"
  e <- exprCore
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
      e <- exprCore
      return (pat, e)

doSemi :: Parser ()
doSemi = do
  optional (token TokNewline)
  symbol ";"
-}
