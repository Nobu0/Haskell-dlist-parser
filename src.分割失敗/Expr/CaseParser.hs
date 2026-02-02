{-# LANGUAGE LambdaCase #-}

module Expr.CaseParser
  ( caseExpr,
    caseAlt,
    caseSep,
  )
where

import Control.Applicative (many, (<|>))
import Expr.AST
import Expr.Combinator (Parser (..), chainl1)
import Expr.ExprCore (exprCore, lambdaExpr, lookAhead, pRecordExpr)
-- import Expr.ExprExtensions (expr)
import Expr.PatternParser (pattern)
import Expr.TokenParser
  ( ident,
    keyword,
    symbol,
    tokenIs,
  )
import Lexer (Token (..))

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

-- ============================================
--  case の区切り
-- ============================================

caseSep :: Parser ()
caseSep = do
  symbol "|"
  return ()

-- ============================================
--  case alternative
--    pattern -> expr
-- ============================================

caseAlt :: Parser CaseAlt
caseAlt = do
  pat <- pattern
  -- ガード付きかどうかを lookAhead で判定
  lookAhead (symbol "|" <|> tokenIs (\case TokArrow -> Just (); _ -> Nothing)) >>= \_ -> do
    guarded pat <|> unguarded pat
  where
    -- ガードなし
    unguarded pat = do
      tokenIs (\case TokArrow -> Just (); _ -> Nothing)
      body <- exprCore
      return (CaseAlt pat body)

    -- ガード付き
    guarded pat = do
      guards <- many1 guardExpr
      return (CaseAltGuard pat guards)

    guardExpr = do
      symbol "|"
      cond <- exprCore
      tokenIs (\case TokArrow -> Just (); _ -> Nothing)
      body <- exprCore
      return (cond, body)

-- ============================================
--  case 式
--    case expr of { alt1 | alt2 | ... }
-- ============================================

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrut <- exprCore
  keyword "of"
  alts <- braces (sepBy1 caseAlt caseSep)
  return (ECase scrut alts)

-- ============================================
--  braces { ... }
-- ============================================

braces :: Parser a -> Parser a
braces p = do
  symbol "{"
  x <- p
  symbol "}"
  return x

-- ============================================
--  sepBy1（ExprCore と同じ実装）
-- ============================================

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)
