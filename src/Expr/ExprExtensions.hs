{-# LANGUAGE LambdaCase #-}

module Expr.ExprExtensions
  ( expr,
    exprTop,
    exprSeq,
    exprSep,
    letExpr,
    ifExpr,
    returnExpr,
    forExpr,
  )
where

import Control.Applicative (many, optional, (<|>))
-- import Expr.CaseParserCore (caseExpr)

-- import Expr.DoParserCore (doExpr)

-- import Expr.ListParserCore (listExpr)

import Data.Functor (void)
import Expr.AST
import Expr.CaseParserCore (caseExprCore)
import Expr.Combinator
import Expr.DoParserCore (doExprCore)
import Expr.ExprCore (exprCore)
import Expr.ListParserCore (listExprCore)
import Expr.PatternParser
import Expr.TokenParser
import Lexer (Token (..))
import MyTrace

expr :: Parser Expr
expr = do
  t <- lookAhead anyToken
  case t of
    TokKeyword "do" -> doExprCore expr
    TokKeyword "case" -> caseExprCore expr
    TokKeyword "let" -> letExpr
    TokKeyword "if" -> ifExpr
    TokKeyword "for" -> forExpr
    TokKeyword "return" -> returnExpr
    TokSymbol "[" -> listExprCore expr
    _ -> exprCore

-- ============================================
--  ディスパッチ式 expr
-- ============================================
{-}
expr :: Parser Expr
expr = do
  t <- lookAhead anyToken
  myTrace ("<< expr next token: " ++ show t)
  case t of
    TokKeyword "do" -> doExpr
    TokKeyword "case" -> caseExpr
    TokKeyword "let" -> letExpr
    TokKeyword "if" -> ifExpr
    TokKeyword "for" -> forExpr
    TokKeyword "return" -> returnExpr
    TokSymbol "[" -> listExpr
    _ -> exprCore
-}

-- ============================================
--  exprTop / exprSeq
-- ============================================

exprTop :: Parser Expr
exprTop = try exprSeq <|> expr

exprSeq :: Parser Expr
exprSeq = do
  es <- sepEndBy1 (try expr) exprSep
  return $ if length es == 1 then head es else ESeq es

exprSep :: Parser ()
exprSep = skipMany1 (symbol ";" <|> newline)

newline :: Parser ()
newline = void (token TokNewline)

skipMany1 :: Parser a -> Parser ()
skipMany1 p = p *> skipMany p

skipMany :: Parser a -> Parser ()
skipMany p = Parser $ \ts ->
  case runParser p ts of
    Just (_, ts') -> runParser (skipMany p) ts'
    Nothing -> Just ((), ts)

-- ============================================
--  let / if / return / for
-- ============================================

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  defs <- def `sepBy1` symbol ";"
  mIn <- optional (keyword "in")
  case mIn of
    Just _ -> ELet defs <$> expr
    Nothing -> return (ELet defs (EVar "__unit__"))

def :: Parser (Pattern, Expr)
def = do
  p <- pattern
  symbol "="
  e <- expr
  return (p, e)

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  cond <- expr
  keyword "then"
  th <- expr
  keyword "else"
  el <- expr
  return (EIf cond th el)

returnExpr :: Parser Expr
returnExpr = do
  keyword "return"
  e <- expr
  return (EReturn e)

forExpr :: Parser Expr
forExpr = do
  keyword "for"
  qs <- sepBy1 qualifier (symbol ",")
  token TokArrow
  body <- expr
  return (EListComp body qs)

qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier = do
  pat <- pattern
  keyword "in"
  src <- expr
  return (QGenerator pat src)

guardQualifier = QGuard <$> expr
