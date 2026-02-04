{-# LANGUAGE LambdaCase #-}

module Parser.Expr.DoParserCore (doExprCore) where

import Control.Applicative (many, optional, (<|>))
import AST.Expr
import Parser.Core.Combinator
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.PatternParser
import Parser.Core.TokenParser
import Lexer.Lexer (Token (..))

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
  -- optional (token TokNewline)
  symbol ";"
