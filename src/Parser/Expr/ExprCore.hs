{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ExprCore
  ( exprCore,
    -- exprLevel1Core,
    -- exprLevel2Core,
    -- exprLevel3Core,
    appExprCore,
    atomCore,
    atomBaseCore,
    parenExprCore,
    tupleExprCore,
    oPsectionCore,
    pRecordExpr,
    lambdaExpr,
  )
where

import AST.Expr
import AST.Expr (BinOp (..), Expr (..))
import Control.Applicative (empty, many, (<|>))
import Control.Monad (guard)
import Data.Functor (void)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.PatternParser (pattern)
import Parser.SQL.SQLParser
import Parser.Type.TypeParser (typeIdent)
import Utils.MyTrace

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

-- ============================================
--  lambdaExpr（ExprCore に戻す）
-- ============================================

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  arg <- pattern
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  bracesV $ do
    body <- exprCore
    return (ELam arg body)

-- ============================================
--  exprCore（純粋な式パーサー）
-- ============================================

exprCore :: Parser Expr
exprCore = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< exprCore next token: " ++ show t)
  -- guard (t /= TokSymbol ";")
  rt <-
    try lambdaExpr
      -- <|> void (token TokEllipsis >> return EPlaceholder)
      <|> try binOpExprCore
      <|> parseSQL
  myTrace ("<< exprCore: rt " ++ show rt)
  return rt

-- <|> exprLevel1Core

-- ===== 演算子階層 =====

binOpExprCore :: Parser Expr
binOpExprCore = exprCmpCore

-- 比較演算子（左結合）
exprCmpCore :: Parser Expr
exprCmpCore = chainl1 exprLevel1Core (binOp [">", "<", ">=", "<=", "==", "/="])

-- 加算・連結・Cons（+, -, ++, :）
-- ここは結合性に応じて分けるのがベスト！
exprLevel1Core :: Parser Expr
exprLevel1Core = do
  e <- chainl1 exprAddSubCore (binOp ["+", "-"])
  -- chainr1 (return e) (binOp ["++", ":"])
  return e

-- 乗算・除算・関数合成（* / .）
exprAddSubCore :: Parser Expr
exprAddSubCore = do
  e <- chainl1 exprLevel3Core (binOp ["*", "/"])
  -- chainr1 (return e) (binOp ["."])
  return e

-- 最下層：関数適用やリテラル、変数など
exprLevel3Core :: Parser Expr
exprLevel3Core =
  try lambdaExpr
    <|> appExprCore

{-}
binOpExprCore :: Parser Expr
binOpExprCore = exprCmpCore

exprCmpCore :: Parser Expr
exprCmpCore = chainl1 exprLevel1Core (binOp [">", "<", ">=", "<=", "==", "/="])

exprLevel1Core :: Parser Expr
exprLevel1Core = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< exprLevel1Core next token: " ++ show t)
  chainl1 exprLevel2Core (binOp ["+", "-", "++", ":"])

exprLevel2Core :: Parser Expr
exprLevel2Core = chainl1 exprLevel3Core (binOp ["*", "/", "."])

exprLevel3Core :: Parser Expr
exprLevel3Core = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< exprLevel3Core next token: " ++ show t)
  try lambdaExpr
    <|> appExprCore
-}

-- ============================================
--  関数適用
-- ============================================

appExprCore :: Parser Expr
appExprCore = do
  f <- atomCore
  args <- many atomCore
  myTrace ("<< appExprCore: f= " ++ show f ++ " args= " ++ show args)
  return (foldl EApp f args)

-- ============================================
--  atom
-- ============================================

atomCore :: Parser Expr
-- atomCore = notFollowedBy badToken *> (parens parenExprCore <|> atomBaseCore)
atomCore = do
  try (parens parenExprCore)
    <|> atomBaseCore

badToken :: Parser ()
badToken =
  choice
    [ symbol "}",
      symbol ";",
      symbol "$",
      tokenIs (\t -> case t of TokLambdaCase -> Just (); _ -> Nothing),
      tokenIs (\t -> case t of TokVRBrace -> Just (); _ -> Nothing)
    ]

parenExprCore :: Parser Expr
parenExprCore = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< parenExprCore next token: " ++ show t)
  try oPsectionCore
    <|> try tupleExprCore
    <|> exprCore

tupleExprCore :: Parser Expr
tupleExprCore = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< tupleExprCore next token: " ++ show t)
  e1 <- exprCore
  symbol ","
  es <- exprCore `sepBy1` symbol ","
  return (ETuple (e1 : es))

oPsectionCore :: Parser Expr
oPsectionCore = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< oPsectionCore next token: " ++ show t)
  try (EOpSectionL <$> operator <*> exprCore)
    <|> (EOpSectionR <$> exprCore <*> operator)

atomBaseCore :: Parser Expr
atomBaseCore = do
  lambdaExpr
    <|> EVar <$> ident
    <|> EInt <$> int
    <|> tunitExpr
    <|> EVarType <$> typeIdent
    <|> (ellipsis >> return EPlaceholder)
    <|> elistExpr
    <|> EString <$> stringLiteralExpr
    <|> EChar <$> charLiteralExpr
    <|> pRecordExpr
    <|> operatorVar
    <|> emptyListExpr

operatorVar :: Parser Expr
operatorVar = do
  op <- satisfyToken isOp
  return (EVar op)
  where
    isOp (TokOperator s)
      | s `elem` [":"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

tunitExpr :: Parser Expr
tunitExpr = do
  symbol "("
  symbol ")"
  return (EUnit)

elistExpr :: Parser Expr
elistExpr = do
  symbol "["
  elems <- exprCore `sepBy` symbol ","
  symbol "]"
  return (EList elems)

ellipsis :: Parser ()
ellipsis = tokenIs (\t -> if t == TokEllipsis then Just () else Nothing)

emptyListExpr :: Parser Expr
emptyListExpr = do
  symbol "["
  symbol "]"
  return (EList [])
