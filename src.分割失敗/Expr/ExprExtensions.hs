{-# LANGUAGE LambdaCase #-}

module Expr.ExprExtensions
  ( expr,
    exprTop,
    exprSeq,
    exprSep,
    letExpr,
    returnExpr,
  )
where

import Control.Applicative (many, optional, (<|>))
import Data.Functor (void)
import Expr.AST
-- import Expr.CaseParser (caseExpr)
import Expr.Combinator
-- import Expr.DoParser (doExpr)
import Expr.ExprCore (exprCore)
-- import Expr.LetParser (letExpr)
import Expr.ListParser (listExpr)
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..))
import MyTrace

-- ============================================
--  トップレベル：expr（拡張構文を合成）
-- ============================================
{-}
expr :: Parser Expr
expr =
      doExpr
  <|> letExpr
  <|> caseExpr
  <|> listExpr
  <|> exprCore
-}

expr :: Parser Expr
expr = do
  t <- lookAhead anyToken
  myTrace ("<< expr next token: " ++ show t)
  -- 拡張構文（exprCore の外側）
  try letExpr
    -- <|> try doExpr
    <|> try listExpr
    -- <|> try caseExpr
    <|> try ifExpr
    -- <|> try returnExpr
    <|> try forExpr
    <|> exprCore

-- ============================================
--  トップレベル構文（あなたの元コードから移植）
-- ============================================

exprTop :: Parser Expr
exprTop = try exprSeq <|> expr

exprSeq :: Parser Expr
exprSeq = do
  myTrace ">> exprSeq"
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
--  if / let / return / for（exprCore から分離）
-- ============================================

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  cond <- expr
  keyword "then"
  thenBranch <- expr
  keyword "else"
  elseBranch <- expr
  return (EIf cond thenBranch elseBranch)

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  defs <- def `sepBy1` symbol ";"
  mIn <- optional (keyword "in")
  case mIn of
    Just _ -> do
      body <- expr
      return (ELet defs body)
    Nothing ->
      return (ELet defs (EVar "__unit__"))

def :: Parser (Pattern, Expr)
def = do
  p <- pattern
  symbol "="
  e <- expr
  return (p, e)

returnExpr :: Parser Expr
returnExpr = do
  keyword "return"
  notFollowedBy (symbol "_")
  e <- expr
  return (EReturn e)

forExpr :: Parser Expr
forExpr = do
  keyword "for"
  qs <- sepBy1 qualifier (symbol ",")
  token TokArrow
  body <- expr
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  keyword "in"
  src <- expr
  return (QGenerator pat src)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> expr
