{-# LANGUAGE LambdaCase #-}

module Parser.Expr.CaseParserCore (caseExprCore, lambdaCaseExpr) where

import AST.Expr
import AST.Pattern
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.Functor (void)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.PatternParser
import Utils.MyTrace

caseExprCore :: Parser Expr -> Parser Expr
caseExprCore expr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  bracesV $ do
    -- skipSeparators
    alts <-
      (sepBy1 (caseAlt expr) caseSep)
    --  <|> sepBy1 (caseAlt expr) caseSep
    return (ECase scrut alts)

lambdaCaseExpr :: Parser Expr -> Parser Expr
lambdaCaseExpr expr = do
  myTrace ("<< lambdaCaseExpr")
  token TokLambdaCase
  bracesV $ do
    t <- lookAhead anyToken
    myTrace ("<< lambdaCaseExpr: next token " ++ show t)
    -- token $ TokKeyword "of"
    -- branches <- sepBy1 (caseBranch expr) (symbol ";")
    branches <- sepBy1 (caseAlt expr) (symbol ";")
    return $ ELambdaCase branches

caseBranch :: Parser Expr -> Parser (Pattern, Expr)
caseBranch expr = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< caseBranch: next token " ++ show t)
  pat <- pPattern
  -- pat <- pattern
  -- t <- lookAhead anyToken
  -- myTrace ("<< caseBranch:2 next token " ++ show t ++ " " ++ show pat)
  token TokArrow
  body <- expr -- NoInfix
  -- t <- lookAhead anyToken
  -- myTrace ("<< caseBranch:2 next token " ++ show t ++ " " ++ show body)
  return (pat, body)

{-}
caseExprCore :: Parser Expr -> Parser Expr
caseExprCore expr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  bracesV $ do
    -- skipSeparators
    alts <-
      (sepBy1 (caseAlt expr) caseSep)
    --  <|> sepBy1 (caseAlt expr) caseSep
    return (ECase scrut alts)
-}

caseSep :: Parser ()
caseSep =
  symbol ";"
    <|> void (token TokNewline) -- newline
    <|> lookAhead patternStart

newline :: Parser ()
newline = void (token TokNewline)

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  -- skipNewlines
  pat <- pattern
  t <- lookAhead anyToken
  myTrace ("<< caseAlt: next token " ++ show t ++ " " ++ show pat)
  guards <- many (guardExpr expr)
  -- t <- lookAhead anyToken
  -- myTrace ("<< caseAlt:2 next token " ++ show t ++ " " ++ show guards)
  -- guards <- many (caseGuard expr)
  case guards of
    [] -> do
      token TokArrow
      body <- expr
      return (CaseAlt pat body)
    _ ->
      return (CaseAltGuard pat guards)

guardExpr :: Parser Expr -> Parser (Expr, Expr)
guardExpr expr = do
  t <- lookAhead anyToken
  myTrace ("<< guardExpr next token: " ++ show t)
  symbol "|"
  t <- lookAhead anyToken
  myTrace ("<< guardExpr2 next token: " ++ show t)
  cond <- expr
  token TokArrow
  t <- lookAhead anyToken
  myTrace ("<< guardExpr3 next token: " ++ show t)
  body <- expr
  myTrace ("<< guardExpr4 body: " ++ show body)
  -- skipSeparators
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
