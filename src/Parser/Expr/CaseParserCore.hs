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
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.PatternParser
import Utils.MyTrace

caseExprCore :: Parser Expr -> Parser Expr
caseExprCore expr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  bracesV $ do
    alts <- caseAltBlock expr -- sepBy1 (try(caseAlt expr)) caseSep
    -- t <- lookAhead anyToken
    myTrace ("<< caseExprCore: alts " ++ show alts)
    return (ECase scrut alts)

caseAltBlock :: Parser Expr -> Parser [CaseAlt]
caseAltBlock expr = sepBy1 (caseAlt expr) caseSep

-- caseAltBlock expr = do
--  first <- caseAlt expr
--  rest <- many (try (caseSep >> lookAhead patternStart >> caseAlt expr))
--  return (first : rest)
{-}
guardedExpr :: Parser Expr -> Parser Expr
guardedExpr expr = do
  t <- lookAhead anyToken
  myTrace("<< guardedExpr: next token "++ show t)
  e <- expr
  t <- optional (lookAhead patternStart)
  case t of
    Just _ -> empty  -- 次がパターンなら失敗して次の caseAlt に進ませる
    Nothing -> return e
-}
{-}
guardedExpr :: Parser Expr -> Parser Expr
guardedExpr expr = do
  t <- lookAhead anyToken
  myTrace ("<< guardedExpr: next token " ++ show t)
  when (isPatternStart t) empty
  expr -- Core

isPatternStart :: Token -> Bool
isPatternStart (TokIdent "_") = True
isPatternStart (TokIdent _) = True
isPatternStart (TokTypeIdent _) = True
isPatternStart (TokNumber _) = True
isPatternStart (TokSymbol "(") = True
isPatternStart _ = False
-}

{-}
caseAltBlock :: Parser Expr -> Parser [CaseAlt]
caseAltBlock expr = do
  alts <- sepBy1 (caseAlt expr) caseSep
  case alts of
    Just [_] -> return alts
    _ -> many (caseAltSimple expr)
-}

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

{-}
caseLambdaBlock :: Parser Expr -> Parser [(Pattern, Expr)]
caseLambdaBlock expr = do
  try $
    sepBy1 (caseBranch expr) (symbol ";")
      <|> caseLambdaBlock2
-}

caseLambdaBlock :: Parser Expr -> Parser [CaseAlt]
caseLambdaBlock expr = do
  sepBy (caseAlt expr) (symbol ";")

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

caseSep :: Parser ()
caseSep =
  symbol ";"
    <|> void (token TokNewline) -- newline
    <|> lookAhead patternStart

caseAlt :: Parser Expr -> Parser CaseAlt
caseAlt expr = do
  -- skipNewlines
  pat <- pattern
  -- t <- lookAhead anyToken
  -- myTrace ("<< caseAlt: next token " ++ show t ++ " " ++ show pat)
  guards <- many (guardExpr expr)
  case guards of
    [] -> do
      token TokArrow
      bracesV $ do
        body <- expr -- guardedExpr expr
        myTrace ("<< caseAlt: " ++ show pat ++ " " ++ show body)
        return (CaseAlt pat body)
    _ -> do
      myTrace ("<< caseAlt: g " ++ show pat ++ " " ++ show guards)
      return (CaseAltGuard pat guards)

guardExpr :: Parser Expr -> Parser (Expr, Expr)
guardExpr expr = do
  t <- lookAhead anyToken
  myTrace ("<< guardExpr next token: " ++ show t)
  symbol "|"
  cond <- expr
  token TokArrow
  body <- expr
  myTrace ("<< guardExpr body: " ++ show body)
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
  myTrace ("<< caseGuard body: " ++ show body)
  return (cond, body)

caseAltSimple :: Parser Expr -> Parser CaseAlt
caseAltSimple expr = do
  t <- lookAhead anyToken
  myTrace ("<< caseAltSimple next token: " ++ show t)
  pat <- pattern
  token TokArrow
  body <- expr
  myTrace ("<< caseAltSimple body: " ++ show body)
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
