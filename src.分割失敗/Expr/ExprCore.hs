{-# LANGUAGE LambdaCase #-}

module Expr.ExprCore
  ( exprCore,
    exprLevel1Core,
    exprLevel2Core,
    exprLevel3Core,
    appExprCore,
    atomCore,
    atomBaseCore,
    parenExprCore,
    tupleExprCore,
    oPsectionCore,
    pRecordExpr,
    lambdaExpr,
    lookAhead,
    binOp,
  )
where

import Control.Applicative (many, (<|>))
import Expr.AST
import Expr.Combinator (Parser (..), chainl1)
import Expr.PatternParser (pattern)
import Expr.TokenParser (ident, int, symbol, tokenIs)
import Lexer (Token (..))
import MyTrace

pRecordExpr :: Parser Expr
pRecordExpr = do
  symbol "{"
  fields <- sepBy1 field (symbol ",")
  symbol "}"
  return (ERecord fields)

field :: Parser (String, Expr)
field = do
  name <- ident
  symbol "="
  value <- exprCore
  return (name, value)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

try :: Parser a -> Parser a
try p = Parser $ \input ->
  case runParser p input of
    Just r -> Just r
    Nothing -> Nothing

parens :: Parser a -> Parser a
parens p = do
  symbol "("
  x <- p
  symbol ")"
  return x

anyToken :: Parser Token
anyToken = Parser $ \input ->
  case input of
    [] -> Nothing
    (t : ts) -> Just (t, ts)

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \input ->
  case runParser p input of
    Just (a, _) -> Just (a, input) -- 成功しても input を消費しない
    Nothing -> Nothing

-- ============================================
--  基本的な choice 実装（元 ExprParser から移植）
-- ============================================

choice1 :: Parser a -> Parser a -> Parser a
choice1 p q = Parser $ \input ->
  case runParser p input of
    Just r -> Just r
    Nothing -> runParser q input

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p : ps) = choice1 p (choice ps)

-- ============================================
--  演算子パーサー（元 ExprParser から移植）
-- ============================================

operator :: Parser String
operator = choice (map (\s -> symbol s >> return s) allOps)
  where
    allOps =
      [ "==",
        "/=",
        ">=",
        "<=",
        "+",
        "-",
        "*",
        "/",
        ">",
        "<"
      ]

-- ============================================
--  binOp（元 ExprParser から移植）
-- ============================================

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \case
  TokOperator op | op `elem` ops -> Just (EBinOp op)
  _ -> Nothing

-- ============================================
--  lambdaExpr（ExprCore に戻す）
-- ============================================

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  arg <- ident
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  body <- exprCore
  return (ELam arg body)

-- ============================================
--  exprCore（純粋な式パーサー）
-- ============================================

exprCore :: Parser Expr
exprCore = do
  t <- lookAhead anyToken
  myTrace ("<< exprCore next token: " ++ show t)
  try lambdaExpr
    <|> try binOpExprCore
    <|> exprLevel1Core

-- ===== 演算子階層 =====

binOpExprCore :: Parser Expr
binOpExprCore = exprCmpCore

exprCmpCore :: Parser Expr
exprCmpCore = chainl1 exprLevel1Core (binOp [">", "<", ">=", "<=", "==", "/="])

exprLevel1Core :: Parser Expr
exprLevel1Core = chainl1 exprLevel2Core (binOp ["+", "-"])

exprLevel2Core :: Parser Expr
exprLevel2Core = chainl1 exprLevel3Core (binOp ["*", "/"])

exprLevel3Core :: Parser Expr
exprLevel3Core =
  try lambdaExpr
    <|> appExprCore

-- ============================================
--  関数適用
-- ============================================

appExprCore :: Parser Expr
appExprCore = do
  f <- atomCore
  args <- many atomCore
  return (foldl EApp f args)

-- ============================================
--  atom
-- ============================================

atomCore :: Parser Expr
atomCore =
  parens parenExprCore
    <|> atomBaseCore

parenExprCore :: Parser Expr
parenExprCore =
  try oPsectionCore
    <|> try tupleExprCore
    <|> exprCore

tupleExprCore :: Parser Expr
tupleExprCore = do
  e1 <- exprCore
  symbol ","
  es <- exprCore `sepBy1` symbol ","
  return (ETuple (e1 : es))

oPsectionCore :: Parser Expr
oPsectionCore =
  try (EOpSectionL <$> operator <*> exprCore)
    <|> try (EOpSectionR <$> exprCore <*> operator)

atomBaseCore :: Parser Expr
atomBaseCore = do
  t <- lookAhead anyToken
  myTrace ("<< atomBaseCore next token: " ++ show t)
  EVar <$> ident
    <|> EInt <$> int
    <|> pRecordExpr
