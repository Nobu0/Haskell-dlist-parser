{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ExprExtensions
  ( expr,
    exprTop,
    -- exprSeq,
    -- exprSep,
    letExpr,
    ifExpr,
    returnExpr,
    forExpr,
    postfix,
    skipNewlines,
  )
where

-- import Expr.CaseParserCore (caseExpr)

-- import Expr.DoParserCore (doExpr)

-- import Expr.ListParserCore (listExpr)

import AST.Expr
-- import AST.Decl
import AST.Pattern
import Control.Applicative (empty, many, optional, some, (<|>))
-- (keyword) -- , whereClause)
import Data.Functor (void)
-- import Decl.DeclParserCore (isEOF)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.CaseParserCore (caseExprCore, lambdaCaseExpr)
import Parser.Expr.DoParserCore (doExprCore)
import Parser.Expr.ExprCore (atomCore, exprCore)
import Parser.Expr.ListParserCore (listExprCore)
import Parser.Expr.PatternParser (pPattern, pattern)
import Parser.SQL.SQLParser
import Utils.MyTrace

-- ============================================
--  exprTop / exprSeq
-- ============================================

-- exprTop :: Parser Expr
-- exprTop = try exprSeq <|> expr
exprTop :: Parser Expr
exprTop = do
  es <- sepEndBy1 expr exprSep
  return $ if length es == 1 then head es else ESeq es

{-}
exprSeq :: Parser Expr
exprSeq = do
  es <- sepEndBy1 expr exprSep
  return $ if length es == 1 then head es else ESeq es
-}

exprSep :: Parser ()
exprSep = skipMany (symbol ";" <|> newline)

expr :: Parser Expr
expr = do
  e <- infixExpr
  myTrace ("<< expr: e " ++ show e)
  return e

infixExpr :: Parser Expr
infixExpr = chainr1 exprTerm infixOp

exprTerm :: Parser Expr
exprTerm = do
  e <- exprNoLoop
  postfix e

exprNoLoop :: Parser Expr
exprNoLoop = do
  e <- exprDispatch
  myTrace ("<< exprNoLoop: e " ++ show e)
  return e

infixOp :: Parser (Expr -> Expr -> Expr)
infixOp = do
  op <- optional operatorB
  myTrace ("<< infixOp: " ++ show op)
  case op of
    Just mop ->
      case parseBinOp mop of
        Just bop -> do
          myTrace ("<< infixOp: parsed as " ++ show bop)
          return (\a b -> EBinOp bop a b)
        Nothing -> do
          myTrace ("<< infixOp: parseBinOp failed for " ++ show mop)
          empty
    Nothing -> empty

postfix :: Expr -> Parser Expr
postfix e = do
  mop <- optional operatorA
  myTrace ("<< postfix: operator = " ++ show mop)
  case mop of
    Just op -> do
      myTrace ("<< postfix: infix operator = " ++ show op)
      rhs <- exprNoLoop
      postfix (EApp (EApp (EVar op) e) rhs)
    Nothing -> do
      mbBinds <- whereClause
      case mbBinds of
        Just binds -> postfix (EWhere e binds)
        Nothing -> return e

{-}
postfix :: Expr -> Parser Expr
postfix e = do
  -- 連続して後置演算子を処理する
  let loop acc = do
        mop <- optional operatorA
        myTrace ("<< postfix: mop = " ++ show mop)
        case mop of
          Just op -> do
            myTrace ("<< postfix: infix operator = " ++ show op)
            rhs <- exprNoLoop
            loop (EApp (EApp (EVar op) acc) rhs)
          Nothing -> do
            mbBinds <- whereClause
            case mbBinds of
              Just binds -> loop (EWhere acc binds)
              Nothing -> return acc
  loop e
-}

-- postfixで参照
operatorA :: Parser String
operatorA = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` ["<$>", "..", ":", "$"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

-- infixで参照
operatorB :: Parser String
operatorB = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` [".", ">>", "++", "<?>", ">>=", "<|>"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

-- すべての構文の入口
exprDispatch :: Parser Expr
exprDispatch = do
  t <- lookAhead anyToken
  myTrace ("<< expr dispatch: " ++ show t)
  case t of
    TokKeyword "do" -> doExprCore expr -- NoLoop
    TokKeyword "case" -> caseExprCore expr -- NoLoop
    -- TokKeyword "let" -> (try letExpr <|> pLetExpr)
    TokKeyword "let" -> letBlock
    TokKeyword "if" -> ifExpr
    TokKeyword "for" -> forExpr
    TokKeyword "return" -> returnExpr
    TokKeyword "sql" -> parseSQL
    TokSymbol "[" -> listExprCore expr -- NoLoop
    -- TokSymbol "{" -> bracesV expr
    -- TokSymbol "\\" -> lambdaExpr
    -- TokSymbol ";" -> empty
    TokLambdaCase -> lambdaCaseExpr expr -- NoLoop
    _ -> exprCore

letBlock :: Parser Expr
letBlock = do
  t <- lookAhead anyToken
  myTrace ("<< letBlock next token: " ++ show t)
  try letExpr <|> pLetExpr

whereClause :: Parser (Maybe [Binding])
whereClause = do
  skipSeparators
  mWhere <- optional (try (keyword "where"))
  case mWhere of
    Just _ -> Just <$> bracesV bindings
    Nothing -> return Nothing

bindings :: Parser [Binding]
bindings = do
  b <- binding
  bs <- many (skipSeparators >> binding)
  -- bs <- many (binding)
  -- bs <- sepEndBy binding (symbol ";") --exprSep
  return (b : bs)

binding :: Parser Binding
binding = try valueBinding <|> funBinding

-- ============================================
--  let / if / return / for
-- ============================================
def :: Parser (Pattern, Expr)
def = do
  p <- pattern
  symbol "="
  e <- expr -- NoLoop
  return (p, e)

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  cond <- expr -- NoLoop
  keyword "then"
  th <- expr -- NoLoop
  keyword "else"
  el <- expr -- NoLoop
  return (EIf cond th el)

returnExpr :: Parser Expr
returnExpr = do
  keyword "return"
  e <- atomCore -- exprNoLoop
  myTrace ("<< return: e " ++ show e)
  return (EReturn e)

forExpr :: Parser Expr
forExpr = do
  keyword "for"
  qs <- sepBy1 qualifier (symbol ",")
  token TokArrow
  body <- expr -- NoLoop
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  keyword "in"
  src <- expr -- NoLoop
  return (QGenerator pat src)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> exprNoLoop

funBinding :: Parser Binding
funBinding = do
  -- optional newline
  t <- lookAhead anyToken
  myTrace ("<< funBinding next token: " ++ show t)
  name <- ident
  -- 次のトークンが "=" なら funBinding ではない
  next <- lookAhead anyToken
  if next == TokOperator "="
    then empty -- ← guard の代わりに empty を使う
    else return ()
  args <- many pattern
  symbol "="
  body <- expr -- NoLoop
  return (PApp (PVar name) args, body)

valueBinding :: Parser Binding
valueBinding = do
  t <- lookAhead anyToken
  myTrace ("<< valueBinding: next token: " ++ show t)
  pat <- pattern
  symbol "="
  body <- expr -- NoLoop
  return (pat, body)

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  -- t <- lookAhead anyToken
  -- myTrace ("<< letExpr: next token: " ++ show t)
  binds <- bindingsBlock
  -- t2 <- lookAhead anyToken
  -- myTrace ("<< letExpr:2 next token: " ++ show t2 ++ " binds " ++ show binds)
  bracesV $ do
    mIn <- optional (keyword "in")
    myTrace ("<< letExpr:3 mIn " ++ show mIn)
    -- optional newline
    case mIn of
      Just _ -> do
        body <- expr -- NoLoop
        return (ELetBlock binds body)
      Nothing ->
        if null binds
          then empty -- ← これが正しい
          else return (ELetBlock binds (EVar "__unit__"))

pLetExpr :: Parser Expr
pLetExpr = do
  keyword "let"
  t <- lookAhead anyToken
  myTrace ("<< pLetExpr next token: " ++ show t)
  pat <- pattern
  symbol "="
  e1 <- expr -- NoLoop
  bracesV $ do
    keyword "in"
    e2 <- expr -- NoLoop
    return (ELet pat e1 e2)

bindingsBlock :: Parser [Binding]
bindingsBlock = do
  braces bindings -- (sepBy binding (symbol ";"))
    <|> bindings -- sepBy binding (symbol ";")
  where
    bindings = do
      f <- binding
      xs <- many binding
      return (f : xs)