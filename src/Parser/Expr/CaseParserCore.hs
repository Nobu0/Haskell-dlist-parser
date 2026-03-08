{-# LANGUAGE LambdaCase #-}

module Parser.Expr.CaseParserCore (caseExprCore, lambdaCaseExpr) where

import AST.Expr
import AST.Pattern
import Control.Applicative (empty, many, optional, some, (<|>))
import Control.Monad (when)
import Data.Functor (void)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore -- (exprCore, exprCoreNoBraces)
import Parser.Expr.PatternParser
import Utils.MyTrace

caseExprCore :: Parser Expr -> Parser Expr
caseExprCore expr = do
  myTrace ("<< caseExprCore:")
  keyword "case"
  scrut <- exprCore -- NoBraces
  keyword "of"
  bracesV $ do
    alts <- caseAltBlock expr -- CoreNoBraces -- expr -- sepBy1 (try(caseAlt expr)) caseSep
    -- t <- lookAhead anyToken
    myTrace (">>*caseExprCore: alts " ++ show alts)
    skipSeparators
    return (ECase scrut alts)

-- caseAltBlock :: Parser Expr -> Parser [CaseAlt]
-- caseAltBlock expr = sepBy1 (caseAlt expr) caseSep

caseAltBlock :: Parser Expr -> Parser [CaseAlt]
caseAltBlock expr = do
  f <- caseAlt expr
  -- t <- lookAhead anyToken
  -- myTrace ("<< caseAltBlock: f " ++ show f ++ " t " ++ show t)
  -- xs <- sepBy1 (caseAlt expr) (symbol ";")
  xs <- many $ caseAlt expr
  myTrace (">>*caseAltBlock: xs " ++ show xs)
  return (f : xs)

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  skipSeparators
  pat <- pattern
  myTrace ("<< caseAlt: pat " ++ show pat)
  -- bracesV $ do
  guards <- bracesV $ many (guardExpr expr)
  case guards of
    [] -> do
      token TokArrow
      -- X bracesV $ do
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
  skipSeparators
  symbol "|"
  cond <- expr
  token TokArrow
  -- X bracesV $ do
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
  bracesV $ do
    t <- lookAhead anyToken
    myTrace ("<< lambdaCaseExpr: next token " ++ show t)
    -- token $ TokKeyword "of"
    -- branches <- sepBy1 (caseBranch expr) (symbol ";")
    branches <- caseLambdaBlock expr -- sepBy1 (caseAlt expr) (symbol ";")
    return $ ELambdaCase branches

caseLambdaBlock :: Parser Expr -> Parser [CaseAlt]
caseLambdaBlock expr = do
  -- sepBy (caseAlt expr) (symbol ";")
  f <- caseAlt expr
  xs <- many (caseAlt expr)
  return (f : xs)

caseBranch :: Parser Expr -> Parser (Pattern, Expr)
caseBranch expr = do
  pat <- pPattern
  token TokArrow
  -- X bracesV $ do
  body <- expr -- NoInfix
  return (pat, body)

caseGuard :: Parser Expr -> Parser (Expr, Expr)
caseGuard expr = do
  t <- lookAhead anyToken
  myTrace ("<< caseGuard next token: " ++ show t)
  symbol "|"
  cond <- expr
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  -- token TokArrow
  body <- expr
  myTrace (">>*caseGuard body: " ++ show body)
  return (cond, body)

caseAltSimple :: Parser Expr -> Parser CaseAlt
caseAltSimple expr = do
  t <- lookAhead anyToken
  myTrace ("<< caseAltSimple next token: " ++ show t)
  pat <- pattern
  token TokArrow
  -- X bracesV $ do
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
