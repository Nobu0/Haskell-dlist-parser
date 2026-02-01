{-# LANGUAGE LambdaCase #-}

module Expr.ExprParser
  ( expr,
    exprTop,
    lambdaExpr,
    letExpr,
    ifExpr,
    caseExpr,
    -- elist,
    -- list,
    range,
    listComp,
    qualifier,
    -- generator,
    -- guardExpr,
    toplevel,
    toplevels,
  )
where

import Control.Applicative
-- import Text.Megaparsec (anySingle, lookAhead)

-- === 式の構文解析 ===

import Data.Functor (void)
import Expr.AST
import Expr.Combinator
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..))
import MyTrace (myTrace, myTraceIO, myTraceShowId)

toplevel :: Parser (String, Expr)
toplevel = do
  name <- myTraceShowId <$> ident -- ← ここで name を強制的に表示！
  args <- many ident
  symbol "="
  exprBody <- expr
  return (name, foldr ELam exprBody args)

choice1 :: Parser a -> Parser a -> Parser a
choice1 p q = Parser $ \input ->
  case runParser p input of
    Just r -> Just r
    Nothing -> runParser q input

choice :: [Parser a] -> Parser a
choice [] = empty
choice (p : ps) = choice1 p (choice ps)

exprTop :: Parser Expr
exprTop = try exprSeq <|> expr

exprSeq :: Parser Expr
exprSeq = do
  myTrace ">> exprSeq"
  es <- sepEndBy1 (try expr) exprSep -- (symbol ";")
  return $ if length es == 1 then head es else ESeq es

exprSep :: Parser ()
exprSep = skipMany1 (symbol ";" <|> newline)

expr :: Parser Expr
expr = do
  t <- lookAhead anyToken
  myTrace ("<< expr next token: " ++ show t)
  try caseExpr
    <|> try doExpr
    <|> try ifExpr
    -- <|> try letExpr
    <|> try lambdaExpr
    <|> try binOpExpr -- ここを追加
    <|> infixExpr

infixExpr :: Parser Expr
infixExpr = exprLevel1

binOpExpr :: Parser Expr
binOpExpr = do
  e <- exprCmp
  -- notFollowedBy (symbol ";")
  -- <?> "unexpected semicolon"
  return e

exprCmp :: Parser Expr
exprCmp = chainl1 exprLevel1 (binOp [">", "<", ">=", "<=", "==", "/="])

exprLevel1 :: Parser Expr
exprLevel1 = chainl1 exprLevel2 (binOp ["+", "-"])

exprLevel2 :: Parser Expr
exprLevel2 = chainl1 exprLevel3 (binOp ["*", "/"])

exprLevel3 :: Parser Expr
exprLevel3 =
  try forExpr
    <|> try returnExpr
    <|> try doExpr
    <|> try ifExpr
    -- <|> try letExpr
    <|> try lambdaExpr
    <|> appExpr

appExpr :: Parser Expr
appExpr = do
  f <- atom
  args <- many atom
  postfix (foldl EApp f args)

atom :: Parser Expr
atom =
  parens parenExpr
    <|> atomBase

parenExpr :: Parser Expr
parenExpr =
  try oPsection
    <|> try tupleExpr
    <|> infixExpr

tupleExpr :: Parser Expr
tupleExpr = do
  e1 <- expr
  _ <- symbol ","
  es <- expr `sepBy1` symbol ","
  return (ETuple (e1 : es))

oPsection :: Parser Expr
oPsection =
  try (EOpSectionL <$> operator <*> infixExpr)
    <|> try (EOpSectionR <$> infixExpr <*> operator)

atomBase :: Parser Expr
atomBase = do
  t <- lookAhead anyToken
  myTrace ("<< atomBase next token: " ++ show t)
  try letExpr
    <|> (token TokEllipsis >> return EPlaceholder)
    <|> EVar <$> ident
    <|> EInt <$> int
    <|> listExpr
    <|> pRecordExpr

postfix :: Expr -> Parser Expr
postfix base =
  pRecordUpdate base
    <|> return base

pRecordUpdate :: Expr -> Parser Expr
pRecordUpdate base = do
  updates <- braces (sepBy field (symbol ","))
  return (ERecordUpdate base updates)

pRecord :: Parser [(String, Expr)]
pRecord = braces (sepBy field (symbol ","))

pRecordExpr :: Parser Expr
pRecordExpr = ERecord <$> pRecord

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrut <- expr
  keyword "of"
  alts <-
    braces (sepBy1 caseAlt caseSep)
      <|> sepBy1 caseAlt caseSep
  return (ECase scrut alts)

caseSep :: Parser ()
caseSep =
  void (symbol ";")
    <|> void newline
    <|> lookAhead patternStart

caseAlt :: Parser CaseAlt
caseAlt = do
  pat <- pattern
  guards <- many caseGuard
  case guards of
    [] -> do
      token TokArrow
      body <- expr
      -- skipMany newline -- ★ 追加
      return (CaseAlt pat body)
    _ -> do
      -- skipMany newline -- ★ 追加
      return (CaseAltGuard pat guards)

newline :: Parser ()
newline = void (token TokNewline)

skipMany1 :: Parser a -> Parser ()
skipMany1 p = p *> skipMany p

skipMany :: Parser a -> Parser ()
skipMany p = Parser $ \ts ->
  case runParser p ts of
    Just (_, ts') -> runParser (skipMany p) ts'
    Nothing -> Just ((), ts)

caseAltEnd :: Parser ()
caseAltEnd =
  symbol ";"
    <|> symbol "}"
    <|> pure ()

caseAltGuarded :: Parser CaseAlt
caseAltGuarded = do
  myTrace ("<< caseAltGuarded")
  pat <- pattern
  guards <- some caseGuard
  return (CaseAltGuard pat guards)

caseGuard :: Parser (Expr, Expr)
caseGuard = do
  symbol "|"
  cond <- expr
  token TokArrow
  body <- expr
  return (cond, body)

caseAltSimple :: Parser CaseAlt
caseAltSimple = do
  myTrace ("<< caseAltSimple")
  pat <- pattern
  token TokArrow
  body <- expr
  return (CaseAlt pat body)

exprStmt :: Parser Stmt
exprStmt = do
  e <- expr
  myTrace "<< exprStmt" >> pure (ExprStmt e)

exprOthers :: Parser Expr
exprOthers = do
  e <- binOpExpr
  -- myTrace ">> exprOthers" >> pure ()
  mdefs <- optional whereClause
  case mdefs of
    Nothing -> return e
    Just defs -> myTrace "<< parsed whereClause" >> (return (ELet defs e))

whereClause :: Parser [(Pattern, Expr)]
whereClause = do
  keyword "where"
  -- myTrace ">> whereClause" >> pure ()
  defs <- braces (sepBy def (symbol ";"))
  myTrace "<< whereClause" >> pure ()
  return defs

toplevels :: Parser [(String, Expr)]
toplevels = sepBy toplevel (symbol ";")

annotatedExpr :: Parser Expr
annotatedExpr = do
  e <- try binOpExpr
  token (TokSymbol "::")
  t <- typeP
  return (EAnn e t)

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  arg <- ident
  token TokArrow
  body <- expr
  return (ELam arg body)

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  -- myTrace ">> letExpr" >> pure ()
  defs <- def `sepBy1` symbol ";"
  -- defs <- some letBinding
  mIn <- optional (keyword "in")
  case mIn of
    Just _ -> do
      body <- expr
      return (ELet defs body)
    Nothing -> do
      -- do式中のlet: let x = 1; return x
      -- bodyは次のstmtで処理されるので、ここでは仮のEVarを入れておく
      return (ELet defs (EVar "__unit__"))

def :: Parser (Pattern, Expr)
def = do
  p <- pattern
  symbol "="
  e <- expr
  return (p, e)

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  -- myTrace ">> ifExpr" >> pure ()
  cond <- binOpExpr
  keyword "then"
  thenBranch <- expr
  keyword "else"
  elseBranch <- expr
  myTrace "<< ifExpr" >> pure ()
  return (EIf cond thenBranch elseBranch)

listExpr :: Parser Expr
listExpr =
  brackets
    ( try listComprehension
        <|> try rangeStep
        <|> try range
        <|> normalList
    )

listItemExpr :: Parser Expr
listItemExpr = exprNoList

atomBaseNoList :: Parser Expr
atomBaseNoList = do
  t <- lookAhead anyToken
  myTrace ("<< atomBaseNoList next token: " ++ show t)
  try letExpr
    <|> (token TokEllipsis >> return EPlaceholder)
    <|> EVar <$> ident
    <|> EInt <$> int
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

appExprNoList :: Parser Expr
appExprNoList = chainl1 atomBaseNoList (return EApp)

exprLevel1NoList :: Parser Expr
exprLevel1NoList = chainl1 exprLevel2NoList (binOp ["+", "-"])

exprLevel2NoList :: Parser Expr
exprLevel2NoList = chainl1 exprLevel3NoList (binOp ["*", "/"])

exprLevel3NoList :: Parser Expr
exprLevel3NoList =
  try forExpr
    <|> try returnExpr
    <|> try doExpr
    <|> try ifExpr
    <|> try lambdaExpr
    <|> appExprNoList

exprCmpNoList :: Parser Expr
exprCmpNoList = chainl1 exprLevel1NoList (binOp [">", "<", ">=", "<=", "==", "/="])

binOpExprNoList :: Parser Expr
binOpExprNoList = exprCmpNoList

exprNoList :: Parser Expr
exprNoList = do
  t <- lookAhead anyToken
  myTrace ("<< exprNoList next token: " ++ show t)
  try caseExpr
    <|> try doExpr
    <|> try ifExpr
    <|> try lambdaExpr
    <|> try binOpExprNoList
    <|> exprLevel1NoList

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

listLiteral :: Parser Expr
listLiteral = do
  elems <- sepBy listItemExpr (symbol ",")
  symbol "]"
  return (EList elems)

listComprehension :: Parser Expr
listComprehension = do
  body <- listItemExpr
  symbol "|"
  qs <- sepBy1 qualifier (symbol ",")
  -- symbol "]"
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  symbol "<-"
  e <- listItemExpr
  return (QGenerator pat e)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> listItemExpr

normalList :: Parser Expr
-- normalList = EList <$> sepBy expr (symbol ",")
normalList = EList <$> sepEndBy listItemExpr (symbol ",")

listComp :: Parser Expr
listComp = do
  body <- listItemExpr
  symbol "|"
  qualifiers <- qualifier `sepBy` symbol ","
  return (EListComp body qualifiers)

forExpr :: Parser Expr
forExpr = do
  keyword "for"
  qs <- sepBy1 qualifier (symbol ",")
  token TokArrow
  body <- expr
  return (EListComp body qs)
  where
    qualifier =
      try genQualifier
        <|> guardQualifier

    genQualifier = do
      pat <- pattern
      keyword "in"
      src <- exprNoArrow
      return (QGenerator pat src)

    guardQualifier =
      QGuard <$> exprNoArrow

exprNoArrow :: Parser Expr
exprNoArrow = do
  t <- lookAhead anyToken
  myTrace ("<< exprNoArrow next token: " ++ show t)
  case t of
    TokArrow -> empty -- ★ これが重要
    _ -> pure ()
  expr

returnExpr :: Parser Expr
returnExpr = do
  keyword "return"
  -- myTrace "<< returnExpr key" >> pure ()
  notFollowedBy (symbol "_") -- ← これで `return _` を防ぐ
  e <- expr
  myTrace "<< returnExpr" >> pure ()
  return (EReturn e)

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \case
  TokOperator op | op `elem` ops -> Just (EBinOp op)
  _ -> Nothing

pOpSection :: Parser Expr
pOpSection = do
  -- t <- lookAhead anyToken
  -- myTrace ("pOpSection next token: " ++ show t)
  parens $
    try (do e <- expr; op <- operator; return (EOpSectionL op e))
      <|> try (do op <- operator; e <- expr; return (EOpSectionR e op))

field :: Parser (String, Expr)
field = do
  name <- ident
  symbol "="
  val <- expr -- exprAtomic
  return (name, val)

operator :: Parser String
operator = choice (map (\s -> symbol s >> return s) allOps)
  where
    allOps = ["==", "/=", ">=", "<=", "+", "-", "*", "/", ">", "<"]

some1 :: Parser a -> Parser [a]
some1 p = do
  x <- p
  xs <- many p
  return (x : xs)

doStmt :: Parser Stmt
doStmt = do
  many (token TokNewline)
  try bindStmt
    <|> try letStmt
    <|> ExprStmt <$> expr

bindStmt :: Parser Stmt
bindStmt = do
  pat <- pattern
  symbol "<-"
  e <- expr
  return (Bind pat e)

letStmt :: Parser Stmt
letStmt = do
  keyword "let"
  binds <- sepBy1 binding (symbol ";")
  return (LetStmt binds)
  where
    binding = do
      pat <- pattern
      symbol "="
      e <- expr
      return (pat, e)

doExpr :: Parser Expr
doExpr = do
  keyword "do"
  symbol "{"
  many (token TokNewline)
  stmts <- doBlock
  many (token TokNewline)
  symbol "}"
  return (EDo stmts)

doBlock :: Parser [Stmt]
doBlock =
  sepBy doStmt doSemi

doSemi :: Parser ()
doSemi = do
  optional (token TokNewline)
  symbol ";"
