{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ExprCore
  ( exprCore,
    appExprCore,
    atomCore,
    atomBaseCore,
    parenExprCore,
    tupleExprCore,
    oPsectionCore,
    pRecordExpr,
  )
where

import AST.Expr
import AST.Expr (BinOp (..), Expr (..))
import Control.Applicative (empty, many, optional, (<|>))
-- import Text.ParserCombinators.ReadP (skipSpaces)

import Data.List (intercalate)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.PatternParser (pattern)
import Parser.SQL.SQLParser
import Parser.Type.TypeParser (typeIdent)
import Utils.MyTrace

-- ============================================
--  exprCore（純粋な式パーサー）
-- ============================================
exprCore :: Parser Expr
exprCore = do
  rt <-
    try lambdaExpr
      <|> try binOpExprCore
      <|> parseSQL
  -- t <- lookAhead anyToken
  -- myTrace (">>*exprCore: rt " ++ show rt)
  return rt

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

-- ===== 演算子階層 =====

binOpExprCore :: Parser Expr
binOpExprCore = do
  exprCmpCore

-- 比較演算子（左結合）
exprCmpCore :: Parser Expr
exprCmpCore = chainl1 exprLevel1Core (binOp [">", "<", ">=", "<=", "==", "/="])

-- 加算・連結・Cons（+, -, ++, :）
-- ここは結合性に応じて分けるのがベスト！
exprLevel1Core :: Parser Expr
exprLevel1Core = do
  e <- chainl1 exprAddSubCore (binOp ["+", "-", "++", ":", "*>", "<$", "<*", "<$>", "<|>"])
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
exprLevel3Core = do
  try lambdaExpr <|> appExprCore

-- ============================================
--  関数適用
-- ============================================

appExprCore :: Parser Expr
appExprCore = do
  f <- atomCore
  -- bracesVO $ do
  args <- many atomCore
  -- myTrace (">>*appExprCore: f= " ++ show f ++ " args= " ++ show args)
  return (foldl EApp f args)

-- ============================================
--  atom
-- ============================================

atomCore :: Parser Expr
atomCore = notFollowedBy badToken *> (parens parenExprCore <|> atomBaseCore)

atomCorex :: Parser Expr
atomCorex = do
  t <- lookAhead anyToken
  case t of
    TokSymbol "}" -> empty
    TokSymbol ";" -> empty
    TokSymbol "$" -> empty
    -- TokVRBrace -> empty
    TokLambdaCase -> empty
    _ ->
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
oPsectionCore = do
  try (EOpSectionL <$> operator <*> exprCore)
    <|> (EOpSectionR <$> exprCore <*> operator)

atomBaseCore :: Parser Expr
atomBaseCore =
  EVar <$> ident
    <|> EInt <$> int
    <|> emptyListExpr
    <|> tunitExpr
    <|> EVarType <$> typeIdent
    <|> (ellipsis >> return EPlaceholder)
    <|> elistExpr
    <|> EString <$> stringLiteralExpr
    <|> EChar <$> charLiteralExpr
    <|> operatorIAsExpr
    <|> pRecordExpr
    <|> parensOperatorVar
    <|> operatorVar

operatorIAsExpr :: Parser Expr
operatorIAsExpr = do
  symbol "`"
  name <- qvarid -- qualified variable identifier, e.g., M.union
  symbol "`"
  return (EVar name)

qvarid :: Parser String
qvarid = do
  mods <- many (try (identI <* symbol "."))
  name <- identI
  return (intercalate "." (mods ++ [name]))

parensOperatorVar :: Parser Expr
parensOperatorVar = do
  op <- parens operatorI
  return (EVar op)

operatorVar :: Parser Expr
operatorVar =
  do
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
  elems <- exprCore `sepBy` (symbol ",")
  optional (symbol ",")
  symbol "]"
  return (EList elems)

ellipsis :: Parser ()
ellipsis = tokenIs (\t -> if t == TokEllipsis then Just () else Nothing)

emptyListExpr :: Parser Expr
emptyListExpr = do
  symbol "["
  symbol "]"
  return (EList [])

-- ほぼ２重定義だけど
lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  arg <- pattern
  myTrace ("<< lambdaExpr arg " ++ show arg)
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  bracesV $ do
    body <- exprCore
    return (ELam arg body)
