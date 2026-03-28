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
-- import Text.ParserCombinators.ReadP (skipSpaces)

import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (find, intercalate)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.PatternParser -- (pattern)
import Parser.SQL.SQLParser
import Parser.Type.TypeParser (typeIdent)
import Utils.MyTrace

data Assoc = LeftAssoc | RightAssoc deriving (Eq, Show)

data BinOpInfo = BinOpInfo
  { opSymbol :: String,
    opConstructor :: BinOp,
    opPrecedence :: Int,
    opAssoc :: Assoc
  }

operatorTable :: [BinOpInfo]
operatorTable =
  [ BinOpInfo "*" BinOpMul 7 LeftAssoc,
    BinOpInfo "/" BinOpDiv 7 LeftAssoc,
    BinOpInfo "+" BinOpAdd 6 LeftAssoc,
    BinOpInfo "-" BinOpSub 6 LeftAssoc,
    BinOpInfo ">" BinOpGt 4 LeftAssoc,
    BinOpInfo "<" BinOpLt 4 LeftAssoc,
    BinOpInfo ">=" BinOpGe 4 LeftAssoc,
    BinOpInfo "<=" BinOpLe 4 LeftAssoc,
    BinOpInfo "==" BinOpEq 4 LeftAssoc,
    BinOpInfo "/=" BinOpNeq 4 LeftAssoc,
    BinOpInfo "&&" BinOpAnd 3 RightAssoc,
    BinOpInfo "||" BinOpOr 2 RightAssoc
  ]

operatorBinOpInfo :: Parser BinOpInfo
operatorBinOpInfo = satisfyToken matchOp
  where
    matchOp (TokOperator s) = find (\op -> opSymbol op == s) operatorTable
    matchOp _ = Nothing

opChainPrec ::
  -- | minimum precedence
  Int ->
  Parser BinOpInfo ->
  (BinOp -> Expr -> Expr -> Expr) ->
  Expr ->
  Parser Expr
opChainPrec minPrec opParser makeExpr lhs = do
  mOp <- optional opParser
  case mOp of
    Just opInfo | opPrecedence opInfo >= minPrec -> do
      let nextMinPrec = case opAssoc opInfo of
            LeftAssoc -> opPrecedence opInfo + 1
            RightAssoc -> opPrecedence opInfo
      rhs <- exprCore
      let combined = makeExpr (opConstructor opInfo) lhs rhs
      opChainPrec minPrec opParser makeExpr combined
    _ -> return lhs

-- 改造バージョン
exprCore :: Parser Expr
exprCore = exprCoreWithOp <|> exprCore2 <|> atomCore

exprCoreWithOp :: Parser Expr
exprCoreWithOp = do
  base <- try fieldExpr <|> appExprCore
  -- skipSeparatorsZ
  opChainPrec 0 operatorBinOpInfo EBinOp base

exprCore2 :: Parser Expr
exprCore2 = do
  rt <-
    try lambdaExpr
      -- <|> appExprCore
      <|> parseSQL
      <|> unaryExpr
  return rt

field :: Parser (String, Expr)
field = do
  name <- ident
  symbol "="
  value <- exprCore
  return (name, value)

pRecordExpr :: Parser Expr
pRecordExpr = do
  symbol "{"
  fields <- sepBy1 field (symbol ",")
  symbol "}"
  return (ERecord fields)

-- ============================================
--  関数適用
-- ============================================
{-}
appExprCore :: Parser Expr
appExprCore = do
  f <- atomCore
  args <- many atomCore
  return (foldl EApp f args)
-}
appExprCore :: Parser Expr
appExprCore = do
  f <- atomCore
  args <- many atomCore
  if null args
    then return f
    else case f of
      EVar _ -> return (foldl EApp f args)
      EApp {} -> return (foldl EApp f args)
      _ -> empty

fieldExpr :: Parser Expr
fieldExpr = do
  base <- atomCore
  rest <-
    many1
      ( do
          token (TokOperator ".")
          field <- ident
          return field
      )
  return (foldl EFieldAccess base rest)

-- ============================================
--  atom
-- ============================================

atomCore :: Parser Expr
atomCore = parens parenExprCore <|> atomBaseCore

-- atomCore = notFollowedBy badToken *> (parens parenExprCore <|> atomBaseCore)

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
  keywordExpr
    <|> EVar <$> ident
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

-- <|> operatorVar

keywordExpr :: Parser Expr
keywordExpr =
  keywordExprTrue <|> keywordExprFalse

keywordExprTrue :: Parser Expr
keywordExprTrue = do
  token (TokKeyword "True")
  return (EBool True)

keywordExprFalse :: Parser Expr
keywordExprFalse = do
  token (TokKeyword "False")
  return (EBool False)

{-}
unaryExpr :: Parser Expr
unaryExpr = do
  symbol "-"
  e <- atomBaseCore
  return (EOpSectionL "-" e) -- または EUnaryMinus e
-}
unaryExpr :: Parser Expr
unaryExpr = do
  symbol "-"
  e <- atomBaseCore
  return (EBinOp BinOpSub (EInt 0) e)

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
  arg <- some simplePattern
  myTrace ("<< lambdaExpr arg " ++ show arg)
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  bracesV $ do
    body <- exprCore
    return (ELam arg body)
