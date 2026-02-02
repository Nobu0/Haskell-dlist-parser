{-# LANGUAGE LambdaCase #-}

module Expr.CaseParserCore (caseExprCore) where

import Control.Applicative (empty, many, some, (<|>))
import Data.Functor (void)
import Expr.AST
import Expr.Combinator
import Expr.ExprCore (exprCore)
import Expr.PatternParser
import Expr.TokenParser
import Lexer (Token (..))
import MyTrace

{-==}
caseExprCore :: Parser Expr -> Parser Expr
caseExprCore expr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  alts <- braces (sepBy1 (caseAlt expr) caseSep)
  return (ECase scrut alts)

caseSep :: Parser ()
caseSep = symbol "|"
===-}

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

{-}

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
-}

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  pat <- pattern
  guards <- many (caseGuard expr)
  case guards of
    [] -> do
      token TokArrow
      body <- expr
      -- skipMany newline -- ★ 追加
      return (CaseAlt pat body)
    _ -> do
      -- skipMany newline -- ★ 追加
      return (CaseAltGuard pat guards)

caseGuard :: Parser Expr -> Parser (Expr, Expr)
caseGuard expr = do
  t <- lookAhead anyToken
  myTrace ("<< caseGuard next token: " ++ show t)
  symbol "|"
  cond <- expr
  token TokArrow
  body <- expr
  return (cond, body)

caseAltGuarded :: Parser Expr -> Parser CaseAlt
caseAltGuarded expr = do
  myTrace ("<< caseAltGuarded")
  pat <- pattern
  guards <- some (caseGuard expr)
  return (CaseAltGuard pat guards)

{-}
caseGuard :: Parser Expr -> Parser (Expr, Expr)
caseGuard expr = do
  symbol "|"
  cond <- expr
  token TokArrow
  body <- expr
  return (cond, body)
-}

caseAltSimple :: Parser Expr -> Parser CaseAlt
caseAltSimple expr = do
  myTrace ("<< caseAltSimple")
  pat <- pattern
  token TokArrow
  body <- expr
  return (CaseAlt pat body)

{-}
caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  pat <- pattern
  lookAhead (symbol "|" <|> tokenIs (\case TokArrow -> Just (); _ -> Nothing))
  guarded pat expr <|> unguarded pat expr

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  pat <- pattern
  -- ガードがあるかどうかを先に判定
  (try (guarded pat expr)) <|> unguarded pat expr

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  pat <- pattern
  tok <- lookAhead anyToken
  myTrace ("<< caseAlt next token: tok=" ++ show tok)
  case tok of
    TokSymbol "|" -> guarded pat expr
    TokArrow -> unguarded pat expr
    _ -> empty
-}

{-}
unguarded :: Pattern -> Parser Expr -> Parser CaseAlt
unguarded pat expr = do
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- expr
  return (CaseAlt pat body)
-}

unguarded :: Pattern -> Parser Expr -> Parser CaseAlt
unguarded pat expr = do
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- expr
  return (CaseAlt pat body)

{-}
guarded :: Pattern -> Parser Expr -> Parser CaseAlt
guarded pat expr = do
  guards <- many1 (guardExpr expr)
  return (CaseAltGuard pat guards)

guarded :: Pattern -> Parser Expr -> Parser CaseAlt
guarded pat expr = do
  guards <- many1 (guardExpr expr)
  return (CaseAltGuard pat guards)
-}

guarded :: Pattern -> Parser Expr -> Parser CaseAlt
guarded pat expr = do
  guards <- many1 (try (guardExpr expr))
  return (CaseAltGuard pat guards)

{-}
guardExpr :: Parser Expr -> Parser (Expr, Expr)
guardExpr expr = do
  symbol "|"
  cond <- expr
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- expr
  return (cond, body)

guardExpr :: Parser Expr -> Parser (Expr, Expr)
guardExpr expr = do
  symbol "|" -- ガード開始
  cond <- expr -- 条件
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- expr -- 本体
  return (cond, body)
-}

guardExpr :: Parser Expr -> Parser (Expr, Expr)
guardExpr expr = try $ do
  symbol "|"
  cond <- expr
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- expr
  return (cond, body)

{-}
caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrut <- exprCore
  keyword "of"
  alts <- braces (sepBy1 caseAlt caseSep)
  return (ECase scrut alts)

caseSep :: Parser ()
caseSep = symbol "|"

caseAlt :: Parser CaseAlt
caseAlt = do
  pat <- pattern
  lookAhead (symbol "|" <|> tokenIs (\case TokArrow -> Just (); _ -> Nothing))
  guarded pat <|> unguarded pat
  where
    unguarded pat = do
      tokenIs (\case TokArrow -> Just (); _ -> Nothing)
      body <- exprCore
      return (CaseAlt pat body)

    guarded pat = do
      guards <- many1 guardExpr
      return (CaseAltGuard pat guards)

    guardExpr = do
      symbol "|"
      cond <- exprCore
      tokenIs (\case TokArrow -> Just (); _ -> Nothing)
      body <- exprCore
      return (cond, body)
-}
