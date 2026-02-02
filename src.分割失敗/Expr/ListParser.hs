{-# LANGUAGE LambdaCase #-}

module Expr.ListParser
  ( listExpr,
    listItemExpr,
    range,
    rangeStep,
    listComprehension,
    normalList,
  )
where

import Control.Applicative (many, optional, (<|>))
import Expr.AST
import Expr.CaseParser (caseSep)
import Expr.Combinator
-- import Expr.DoParser (doSemi)
import Expr.ExprCore (binOp, exprCore, lambdaExpr, pRecordExpr)
-- import Expr.ExprExtensions (expr)
import Expr.PatternParser
import Expr.TokenParser
import Lexer (Token (..))
import MyTrace

-- ============================================
--  NoList 系（listExpr 内部専用の式パーサー）
-- ============================================

listItemExpr :: Parser Expr
listItemExpr = exprNoList

exprNoList :: Parser Expr
exprNoList = do
  t <- lookAhead anyToken
  myTrace ("<< exprNoList next token: " ++ show t)
  try caseExprNoList
    <|> try doExprNoList
    <|> try ifExprNoList
    <|> try lambdaExpr
    <|> try binOpExprNoList
    <|> exprLevel1NoList

-- ===== NoList: 演算子階層 =====

binOpExprNoList :: Parser Expr
binOpExprNoList = exprCmpNoList

exprCmpNoList :: Parser Expr
exprCmpNoList = chainl1 exprLevel1NoList (binOp [">", "<", ">=", "<=", "==", "/="])

exprLevel1NoList :: Parser Expr
exprLevel1NoList = chainl1 exprLevel2NoList (binOp ["+", "-"])

exprLevel2NoList :: Parser Expr
exprLevel2NoList = chainl1 exprLevel3NoList (binOp ["*", "/"])

exprLevel3NoList :: Parser Expr
exprLevel3NoList =
  try forExprNoList
    <|> try returnExprNoList
    <|> try doExprNoList
    <|> try ifExprNoList
    <|> try lambdaExpr
    <|> appExprNoList

-- ===== NoList: 関数適用 =====

appExprNoList :: Parser Expr
appExprNoList = chainl1 atomBaseNoList (return EApp)

-- ===== NoList: atom =====

atomBaseNoList :: Parser Expr
atomBaseNoList = do
  t <- lookAhead anyToken
  myTrace ("<< atomBaseNoList next token: " ++ show t)
  try letExprNoList
    <|> (token TokEllipsis >> return EPlaceholder)
    <|> EVar
      <$> ident
    <|> EInt
      <$> int
    <|> pRecordExpr
    <|> tupleOrParenNoList

tupleOrParenNoList :: Parser Expr
tupleOrParenNoList = do
  symbol "("
  e1 <- exprNoList
  ( do
      symbol ","
      es <- sepBy1 exprNoList (symbol ",")
      symbol ")"
      return (ETuple (e1 : es))
    )
    <|> (symbol ")" >> return e1)

-- ============================================
--  NoList: let
-- ============================================

letExprNoList :: Parser Expr
letExprNoList = do
  keyword "let"
  defs <- defNoList `sepBy1` symbol ";"
  mIn <- optional (keyword "in")
  case mIn of
    Just _ -> do
      body <- exprNoList
      return (ELet defs body)
    Nothing ->
      return (ELet defs (EVar "__unit__"))

defNoList :: Parser (Pattern, Expr)
defNoList = do
  p <- pattern
  symbol "="
  e <- exprNoList
  return (p, e)

-- ============================================
--  NoList: if
-- ============================================

ifExprNoList :: Parser Expr
ifExprNoList = do
  keyword "if"
  cond <- exprNoList
  keyword "then"
  thenBranch <- exprNoList
  keyword "else"
  elseBranch <- exprNoList
  return (EIf cond thenBranch elseBranch)

-- ============================================
--  NoList: case
-- ============================================

caseExprNoList :: Parser Expr
caseExprNoList = do
  keyword "case"
  scrut <- exprNoList
  keyword "of"
  alts <- braces (sepBy1 caseAltNoList caseSep)
  return (ECase scrut alts)

caseAltNoList :: Parser CaseAlt
caseAltNoList = do
  pat <- pattern
  guards <- many caseGuardNoList
  case guards of
    [] -> do
      token TokArrow
      body <- exprNoList
      return (CaseAlt pat body)
    _ ->
      return (CaseAltGuard pat guards)

caseGuardNoList :: Parser (Expr, Expr)
caseGuardNoList = do
  symbol "|"
  cond <- exprNoList
  token TokArrow
  body <- exprNoList
  return (cond, body)

-- ============================================
--  NoList: do
-- ============================================

doExprNoList :: Parser Expr
doExprNoList = do
  keyword "do"
  symbol "{"
  many (token TokNewline)
  stmts <- sepBy doStmtNoList doSemi
  many (token TokNewline)
  symbol "}"
  return (EDo stmts)

doStmtNoList :: Parser Stmt
doStmtNoList =
  try bindStmtNoList
    <|> try letStmtNoList
    <|> ExprStmt
      <$> exprNoList

bindStmtNoList :: Parser Stmt
bindStmtNoList = do
  pat <- pattern
  symbol "<-"
  e <- exprNoList
  return (Bind pat e)

letStmtNoList :: Parser Stmt
letStmtNoList = do
  keyword "let"
  binds <- sepBy1 binding (symbol ";")
  return (LetStmt binds)
  where
    binding = do
      pat <- pattern
      symbol "="
      e <- exprNoList
      return (pat, e)

-- ============================================
--  NoList: return
-- ============================================

returnExprNoList :: Parser Expr
returnExprNoList = do
  keyword "return"
  notFollowedBy (symbol "_")
  e <- exprNoList
  return (EReturn e)

-- ============================================
--  NoList: for
-- ============================================

forExprNoList :: Parser Expr
forExprNoList = do
  keyword "for"
  qs <- sepBy1 qualifierNoList (symbol ",")
  token TokArrow
  body <- exprNoList
  return (EListComp body qs)

qualifierNoList :: Parser Qualifier
qualifierNoList =
  try genQualifierNoList
    <|> guardQualifierNoList

genQualifierNoList :: Parser Qualifier
genQualifierNoList = do
  pat <- pattern
  keyword "in"
  src <- exprNoList
  return (QGenerator pat src)

guardQualifierNoList :: Parser Qualifier
guardQualifierNoList = QGuard <$> exprNoList

-- ============================================
--  listExpr 本体
-- ============================================

listExpr :: Parser Expr
listExpr =
  brackets
    ( try listComprehension
        <|> try rangeStep
        <|> try range
        <|> normalList
    )

-- ===== range =====

rangeStep :: Parser Expr
rangeStep = try $ do
  start <- listItemExpr
  symbol ","
  step <- listItemExpr
  symbol ".."
  end <- listItemExpr
  return (ERangeStep start step end)

range :: Parser Expr
range = try $ do
  start <- listItemExpr
  symbol ".."
  end <- listItemExpr
  return (ERange start end)

-- ===== list comprehension =====

listComprehension :: Parser Expr
listComprehension = do
  body <- listItemExpr
  symbol "|"
  qs <- sepBy1 qualifierNoList (symbol ",")
  return (EListComp body qs)

-- ===== normal list =====

normalList :: Parser Expr
normalList = EList <$> sepEndBy listItemExpr (symbol ",")

doSemi :: Parser ()
doSemi = do
  optional (token TokNewline)
  symbol ";"
