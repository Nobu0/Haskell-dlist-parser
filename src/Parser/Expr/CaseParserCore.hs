{-# LANGUAGE LambdaCase #-}

module Parser.Expr.CaseParserCore (caseExprCore) where

import Control.Applicative (empty, many, optional, some, (<|>))
import Data.Functor (void)
import AST.Expr
import AST.Pattern
import Parser.Core.Combinator
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.PatternParser
import Parser.Core.TokenParser
import Lexer.Lexer (Token (..))
import Utils.MyTrace

caseExprCore :: Parser Expr -> Parser Expr
caseExprCore expr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  alts <-
    braces (sepBy1 (caseAlt expr) caseSep)
      <|> sepBy1 (caseAlt expr) caseSep
  return (ECase scrut alts)

caseSep :: Parser ()
caseSep =
  symbol ";"
    <|> void (token TokNewline) -- newline
    <|> lookAhead patternStart

newline :: Parser ()
newline = void (token TokNewline)

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  skipNewlines
  pat <- pattern
  guards <- many (guardExpr expr)
  -- guards <- many (caseGuard expr)
  case guards of
    [] -> do
      token TokArrow
      body <- expr
      return (CaseAlt pat body)
    _ ->
      return (CaseAltGuard pat guards)

skipNewlines :: Parser ()
skipNewlines = do
  _ <- many (tokenIs (\t -> if t == TokNewline then Just () else Nothing))
  return ()

guardExpr :: Parser Expr -> Parser (Expr, Expr)
guardExpr expr = do
  skipNewlines
  symbol "|"
  cond <- expr
  token TokArrow
  body <- expr
  return (cond, body)

caseGuard :: Parser Expr -> Parser (Expr, Expr)
caseGuard expr = do
  -- optional (token TokNewline)
  t <- lookAhead anyToken
  myTrace ("<< caseGuard next token: " ++ show t)
  symbol "|"
  cond <- expr
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  -- token TokArrow
  body <- expr
  -- optional (token TokNewline)
  return (cond, body)

caseAltSimple :: Parser Expr -> Parser CaseAlt
caseAltSimple expr = do
  t <- lookAhead anyToken
  myTrace ("<< caseAltSimple next token: " ++ show t)
  pat <- pattern
  token TokArrow
  body <- expr
  return (CaseAlt pat body)

unguarded :: Pattern -> Parser Expr -> Parser CaseAlt
unguarded pat expr = do
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- expr
  return (CaseAlt pat body)

guarded :: Pattern -> Parser Expr -> Parser CaseAlt
guarded pat expr = do
  guards <- many1 (try (guardExpr expr))
  return (CaseAltGuard pat guards)
