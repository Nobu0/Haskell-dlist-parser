{-# LANGUAGE LambdaCase #-}

module Parser.Expr.CaseParserCore (caseExprCore, lambdaCaseExpr) where

import AST.Expr
import AST.Pattern
import Control.Applicative (empty, many, optional, some, (<|>))
import Control.Monad (when)
import Data.Functor (void)
-- (exprCore, exprCoreNoBraces)

import Data.Text.Internal.Encoding (skipIncomplete)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore
import Parser.Expr.PatternParser
import Utils.MyTrace

caseExprCore :: Parser Expr -> Parser Expr
caseExprCore expr = do
  myTrace ("<< caseExprCore:")
  keyword "case"
  bracesV $ do
    scrut <- exprCore -- NoBraces
    keyword "of"
    bracesV $ do
      alts <- caseAltBlock expr -- CoreNoBraces -- expr -- sepBy1 (try(caseAlt expr)) caseSep
      -- t <- lookAhead anyToken
      myTrace (">>*caseExprCore: alts " ++ show alts)
      return (ECase scrut alts)

-- caseAltBlock :: Parser Expr -> Parser [CaseAlt]
-- caseAltBlock expr = sepBy1 (caseAlt expr) caseSep

caseAltBlock :: Parser Expr -> Parser [CaseAlt]
caseAltBlock expr = do
  f <- caseAlt expr
  xs <- many $ caseAlt expr
  myTrace (">>*caseAltBlock: xs " ++ show xs)
  return (f : xs)

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  optional (caseSep)
  pat <- pattern
  myTrace ("<< caseAlt: pat " ++ show pat)
  guards <- many (guardExpr expr)
  case guards of
    [] -> do
      token TokArrow
      skipNL
      body <- expr -- guardedExpr expr
      myTrace (">>*caseAlt: pat " ++ show pat ++ " body " ++ show body)
      return (CaseAlt pat body)
    _ -> do
      myTrace (">>*caseAlt: g " ++ show pat ++ " " ++ show guards)
      return (CaseAltGuard pat guards)

guardExpr :: Parser Expr -> Parser (Expr, Expr)
guardExpr expr = do
  t <- lookAhead anyToken
  myTrace ("<< guardExpr next token: " ++ show t)
  skipNL
  symbol "|"
  cond <- expr
  token TokArrow
  skipNL
  body <- expr
  myTrace (">>*guardExpr body: " ++ show body)
  return (cond, body)

caseSep :: Parser ()
caseSep =
  symbol ";"
    <|> void (token TokNewline) -- newline
    <|> lookAhead patternStart

lambdaCaseExpr :: Parser Expr -> Parser Expr
lambdaCaseExpr expr = do
  myTrace ("<< lambdaCaseExpr")
  token TokLambdaCase
  -- bracesV $ do
  t <- lookAhead anyToken
  myTrace ("<< lambdaCaseExpr: next token " ++ show t)
  branches <- caseLambdaBlock expr -- sepBy1 (caseAlt expr) (symbol ";")
  return $ ELambdaCase branches

caseLambdaBlock :: Parser Expr -> Parser [CaseAlt]
caseLambdaBlock expr = do
  f <- caseAlt expr
  xs <- many (caseAlt expr)
  return (f : xs)

caseBranch :: Parser Expr -> Parser (Pattern, Expr)
caseBranch expr = do
  pat <- pPattern
  token TokArrow
  body <- expr -- NoInfix
  return (pat, body)

caseGuard :: Parser Expr -> Parser (Expr, Expr)
caseGuard expr = do
  -- skipNL
  t <- lookAhead anyToken
  myTrace ("<< caseGuard next token: " ++ show t)
  symbol "|"
  cond <- expr
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- expr
  myTrace (">>*caseGuard body: " ++ show body)
  return (cond, body)

caseAltSimple :: Parser Expr -> Parser CaseAlt
caseAltSimple expr = do
  -- skipNL
  t <- lookAhead anyToken
  myTrace ("<< caseAltSimple next token: " ++ show t)
  pat <- pattern
  token TokArrow
  body <- expr
  myTrace (">>*caseAltSimple body: " ++ show body)
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
