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
import Parser.Expr.ExprCore (exprCore, atomCore)
import Parser.Expr.ListParserCore (listExprCore)
import Parser.Expr.PatternParser (pPattern, pattern)
import Parser.SQL.SQLParser
import Utils.MyTrace

-- ============================================
--  exprTop / exprSeq
-- ============================================

exprTop :: Parser Expr
exprTop = try exprSeq <|> expr

exprSeq :: Parser Expr
exprSeq = do
  es <- sepEndBy1 expr exprSep
  return $ if length es == 1 then head es else ESeq es

exprSep :: Parser ()
exprSep = skipMany1 (symbol ";" <|> newline)

expr :: Parser Expr
expr = do
  t <- lookAhead anyToken
  myTrace ("<< expr: next token " ++ show t)
  rt <- infixExpr
  return rt

infixExpr :: Parser Expr
infixExpr = chainl1 exprTerm infixOp

exprTerm :: Parser Expr
exprTerm = do
  t <- lookAhead anyToken
  myTrace ("<< exprTerm: next token " ++ show t)
  e <- exprNoLoop
  postfix e

exprNoLoop :: Parser Expr
exprNoLoop = exprDispatch

infixOp :: Parser (Expr -> Expr -> Expr)
infixOp = do
  t <- lookAhead anyToken
  myTrace ("<< infixOp: next token " ++ show t)
  op <- operatorA
  myTrace ("<< infixOp: " ++ show op)
  return (\a b -> EApp (EApp (EVar op) a) b)

-- すべての構文の入口
exprDispatch :: Parser Expr
exprDispatch = do
  t <- lookAhead anyToken
  myTrace ("<< expr dispatch: " ++ show t)
  case t of
    TokKeyword "do" -> doExprCore exprNoLoop
    TokKeyword "case" -> caseExprCore exprNoLoop
    -- TokKeyword "let" -> (try letExpr <|> pLetExpr)
    TokKeyword "let" -> letBlock
    TokKeyword "if" -> ifExpr
    TokKeyword "for" -> forExpr
    TokKeyword "return" -> returnExpr
    TokKeyword "sql" -> parseSQL
    TokSymbol "[" -> listExprCore exprNoLoop
    -- TokSymbol "\\" -> lambdaExpr
    TokLambdaCase -> lambdaCaseExpr exprNoLoop
    _ -> exprCore

letBlock :: Parser Expr
letBlock = do
  t <- lookAhead anyToken
  myTrace ("<< letBlock next token: " ++ show t)
  try letExpr <|> pLetExpr

postfix :: Expr -> Parser Expr
postfix e = do
  mtok <- optional (lookAhead anyToken)
  case mtok of
    Just (TokOperator _) -> do
      op <- operatorI
      myTrace ("<< postfix: infix operator = " ++ show op)
      rhs <- exprNoLoop
      postfix (EApp (EApp (EVar op) e) rhs)
    Just (TokKeyword "where") -> do
      mbBinds <- whereClause
      case mbBinds of
        Just binds -> postfix (EWhere e binds)
        Nothing -> return e
    _ -> return e
{-}
whereClause :: Parser (Maybe [Binding])
whereClause = do
  skipSeparators
  t <- lookAhead anyToken
  myTrace ("<< whereClause: next token " ++ show t)
  try (keyword "where" >> bindings >>= \bs -> return (Just bs))
    <|> return Nothing
-}

whereClause :: Parser (Maybe [Binding])
whereClause = do
  skipSeparators
  t <- lookAhead anyToken
  myTrace ("<< whereClause: next token " ++ show t)
  mWhere <- optional (try (keyword "where"))
  case mWhere of
    Just _  -> Just <$> bindings
    Nothing -> return Nothing

bindings :: Parser [Binding]
bindings = do
  bracesV $ do
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
  e <- exprNoLoop
  return (p, e)

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  cond <- exprNoLoop
  keyword "then"
  th <- exprNoLoop
  keyword "else"
  el <- exprNoLoop
  return (EIf cond th el)

returnExpr :: Parser Expr
returnExpr = do
  keyword "return"
  e <- atomCore -- exprNoLoop
  myTrace("<< return: e "++ show e)
  return (EReturn e)

forExpr :: Parser Expr
forExpr = do
  keyword "for"
  qs <- sepBy1 qualifier (symbol ",")
  token TokArrow
  body <- exprNoLoop
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  keyword "in"
  src <- exprNoLoop
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
  body <- exprNoLoop
  return (PApp (PVar name) args, body)

valueBinding :: Parser Binding
valueBinding = do
  t <- lookAhead anyToken
  myTrace ("<< valueBinding: next token: " ++ show t)
  pat <- pattern
  symbol "="
  body <- exprNoLoop
  return (pat, body)

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  t <- lookAhead anyToken
  myTrace ("<< letExpr: next token: " ++ show t)
  binds <- bindingsBlock
  t2 <- lookAhead anyToken
  myTrace ("<< letExpr:2 next token: " ++ show t2++ " binds "++ show binds)
  bracesV $ do
    mIn <- optional (keyword "in")
    myTrace ("<< letExpr:3 mIn " ++ show mIn)
    -- optional newline
    case mIn of
      Just _ -> do
        body <- exprNoLoop
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
  e1 <- exprNoLoop
  bracesV $ do
    keyword "in"
    e2 <- exprNoLoop
    return (ELet pat e1 e2)

bindingsBlock :: Parser [Binding]
bindingsBlock = do
  -- optional (token TokNewline)
  braces (sepBy binding (symbol ";"))
    <|> sepBy binding (symbol ";")
