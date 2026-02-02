{-# LANGUAGE LambdaCase #-}

module Expr.CaseParser
  ( caseExpr,
  )
where

import Control.Applicative
-- import Text.Megaparsec (anySingle, lookAhead)

-- === 式の構文解析 ===

import Data.Functor (void)
import Expr.AST
import Expr.Combinator
import Expr.ListParser (listExpr)
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..))
import MyTrace (myTrace, myTraceIO, myTraceShowId)

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  alts <-
    braces (sepBy1 caseAlt caseSep)
      <|> sepBy1 caseAlt caseSep
  return (ECase scrut alts)

caseSep :: Parser ()
caseSep =
  void (symbol ";")
    <|> void newline
    <|> lookAhead patternStart

caseAlt :: Parser CaseAlt
caseAlt = do
  pat <- pattern
  guards <- many caseGuard
  case guards of
    [] -> do
      token TokArrow
      body <- expr
      -- skipMany newline -- ★ 追加
      return (CaseAlt pat body)
    _ -> do
      -- skipMany newline -- ★ 追加
      return (CaseAltGuard pat guards)

newline :: Parser ()
newline = void (token TokNewline)

skipMany1 :: Parser a -> Parser ()
skipMany1 p = p *> skipMany p

skipMany :: Parser a -> Parser ()
skipMany p = Parser $ \ts ->
  case runParser p ts of
    Just (_, ts') -> runParser (skipMany p) ts'
    Nothing -> Just ((), ts)

caseAltEnd :: Parser ()
caseAltEnd =
  symbol ";"
    <|> symbol "}"
    <|> pure ()

caseAltGuarded :: Parser CaseAlt
caseAltGuarded = do
  myTrace ("<< caseAltGuarded")
  pat <- pattern
  guards <- some caseGuard
  return (CaseAltGuard pat guards)

caseGuard :: Parser (Expr, Expr)
caseGuard = do
  symbol "|"
  cond <- expr
  token TokArrow
  body <- expr
  return (cond, body)

caseAltSimple :: Parser CaseAlt
caseAltSimple = do
  myTrace ("<< caseAltSimple")
  pat <- pattern
  token TokArrow
  body <- expr
  return (CaseAlt pat body)
