{-# LANGUAGE LambdaCase #-}

module Parser.Expr.DoParserCore (doExprCore) where

import AST.Expr
import Control.Applicative (empty, many, optional, (<|>))
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.PatternParser
import Utils.MyTrace

doExprCore :: Parser Expr -> Parser Expr
doExprCore expr = do
  keyword "do"
  -- many (token TokNewline)
  stmts <- bracesV (doBlock expr)
  -- many (token TokNewline)
  return (EDo stmts)

doBlock :: Parser Expr -> Parser [Stmt]
doBlock expr = do
  sepBy (doStmt expr) doSemi

doStmt :: Parser Expr -> Parser Stmt
doStmt expr = do
  many (token TokNewline)
  t <- lookAhead anyToken
  myTrace ("<< doStmt next token: " ++ show t)
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
  t <- lookAhead anyToken
  myTrace ("<< letStmt:binding next token: " ++ show t)
  case t of
    TokKeyword "in" -> empty
    _ -> return (LetStmt binds)
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
