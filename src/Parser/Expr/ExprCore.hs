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
    -- lambdaExpr,
  )
where

import AST.Expr
import AST.Expr (BinOp (..), Expr (..))
import Control.Applicative (empty, many, optional, (<|>))
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.PatternParser (pattern)
import Parser.SQL.SQLParser
import Parser.Type.TypeParser (typeIdent)
-- import Text.ParserCombinators.ReadP (skipSpaces)
import Utils.MyTrace

-- isSep TokVLBrace = Just ()
-- isSep TokVRBrace = Just ()

skipSeparatorsZ :: Parser ()
skipSeparatorsZ = do
  _ <- many (tokenIs isSep)
  return ()
  where
    isSep TokNewline = Just ()
    isSep (TokSymbol ";") = Just ()
    isSep _ = Nothing

{-}
opAltChain :: Expr -> Parser Expr
opAltChain lhs = do
  optional $ token TokVLBrace
  hasOp <- optional operatorZ
  case hasOp of
    Just _ -> do
      rhs <- exprCore
      optional $ token TokVRBrace
      skipSeparatorsZ
      t <- lookAhead anyToken
      myTrace (">>*opAltChain t " ++ show t ++ " lhs " ++ show lhs ++ " rhs " ++ show rhs)
      let combined = EBinOp BinOpAlt lhs rhs
      opAltChain combined
    Nothing -> do
      myTrace (">>*opAltChain Nop lhs " ++ show lhs)
      return lhs

-- 今までの関数
exprCore :: Parser Expr
exprCore = do
  bracesvExpr <|> exprCore2

bracesvExpr :: Parser Expr
bracesvExpr = do
  token TokVLBrace
  e <- exprCore >>= opAltChain
  token TokVRBrace
  return e

operatorZ :: Parser String
operatorZ = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` ["<|>","<$>"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing
-}

bracesvExpr :: Parser Expr
bracesvExpr = do
  token TokVLBrace
  base <- exprCore
  token TokVLBrace
  e <- opChain operatorBinOp EBinOp base
  token TokVRBrace
  token TokVRBrace
  skipSeparatorsZ
  return e

-- 改造バージョン
exprCore :: Parser Expr
exprCore = do
  bracesvExpr <|> exprCoreWithOp

exprCoreWithOp :: Parser Expr
exprCoreWithOp = do
  base <- exprCore2
  skipSeparatorsZ
  opChain operatorBinOp EBinOp base

opChain :: Parser BinOp -> (BinOp -> Expr -> Expr -> Expr) -> Expr -> Parser Expr
opChain opParser makeExpr lhs = do
  hasOp <- optional opParser
  case hasOp of
    Just op -> do
      rhs <- exprCore
      skipSeparatorsZ
      let combined = makeExpr op lhs rhs
      myTrace ("<<--- opChain " ++ show combined)
      opChain opParser makeExpr combined
    Nothing -> return lhs

operatorBinOp :: Parser BinOp
operatorBinOp = satisfyToken matchOp
  where
    matchOp (TokOperator s) = lookup s operatorTable
    matchOp _ = Nothing

operatorTable :: [(String, BinOp)]
operatorTable =
  [ ("<|>", BinOpAlt),
    ("<$>", BinOpMap),
    ("&&", BinOpAnd),
    (">>=", BinOpBind)
  ]

exprCore2 :: Parser Expr
exprCore2 = do
  rt <-
    try binOpExprCore
      <|> parseSQL
  myTrace (">>*exprCore: rt " ++ show rt)
  -- e <- opAltChain rt
  return rt

{-}
exprCore2 :: Parser Expr
exprCore2 = do
  skipSeparatorsZ
  e <- exprCore2 -- layoutExpr -- try (parens exprCore2) <|> exprCore2
  skipSeparatorsZ
  t <- lookAhead anyToken
  myTrace (">>*exprCore: e " ++ show e ++ " t " ++ show t)
  return e

layoutExpr :: Parser Expr
layoutExpr = do
  t <- lookAhead anyToken
  myTrace ("<< layoutExpr: t " ++ show t)
  case t of
    TokVLBrace -> bracesv exprCore
    -- TokSymbol "(" -> parens exprCore2
    _ -> exprCore2
-}
-- isOp (TokVLBrace) = empty
-- isOp (TokVRBrace) = empty
{-}
bracesExpr :: Parser Expr
bracesExpr = do
  myTrace ("<< exprCore1: 1")
  token TokVLBrace
  myTrace ("<< exprCore1: 2")
  e <- exprCore
  skipSeparatorsZ
  t <- lookAhead anyToken
  myTrace ("<< exprCore1: 3 " ++ show e ++ " t " ++ show t)
  token TokVRBrace
  myTrace (">>*exprCore1: 4 " ++ show e)
  return e
-}

-- ============================================
--  exprCore（純粋な式パーサー）
-- ============================================

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
  bracesVO $ do
    exprCmpCore

-- 比較演算子（左結合）
exprCmpCore :: Parser Expr
exprCmpCore = chainl1 exprLevel1Core (binOp [">", "<", ">=", "<=", "==", "/="])

-- 加算・連結・Cons（+, -, ++, :）
-- ここは結合性に応じて分けるのがベスト！
exprLevel1Core :: Parser Expr
exprLevel1Core = do
  e <- chainl1 exprAddSubCore (binOp ["+", "-", "++", ":", "*>", "<$", "<*", "<$>"])
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
  appExprCore

-- ============================================
--  関数適用
-- ============================================

appExprCore :: Parser Expr
appExprCore = do
  f <- atomCore
  -- bracesVO $ do
  args <- many atomCore
  myTrace (">>*appExprCore: f= " ++ show f ++ " args= " ++ show args)
  return (foldl EApp f args)

-- let rt = (foldl EApp f args)
-- return rt

-- ============================================
--  atom
-- ============================================

xatomCore :: Parser Expr
xatomCore = notFollowedBy badToken *> (parens parenExprCore <|> atomBaseCore)

atomCore :: Parser Expr
atomCore = do
  t <- lookAhead anyToken
  case t of
    TokSymbol "}" -> empty
    TokSymbol ";" -> empty
    TokSymbol "$" -> empty
    TokVRBrace -> empty
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
  do
    EVar <$> ident
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
