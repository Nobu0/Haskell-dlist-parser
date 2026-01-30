{-# LANGUAGE LambdaCase #-}

module Expr.ExprParser
  ( expr,
    exprTop,
    lambdaExpr,
    letExpr,
    ifExpr,
    caseExpr,
    elist,
    list,
    range,
    listComp,
    qualifier,
    generator,
    guardExpr,
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

exprTop :: Parser Expr
exprTop = try exprSeq <|> expr

exprSeq :: Parser Expr
exprSeq = do
  myTrace ">> exprSeq"
  es <- sepEndBy1 (try expr) (symbol ";")
  return $ if length es == 1 then head es else ESeq es

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

exprLevel3 :: Parser Expr
exprLevel3 =
  -- try caseExpr
  try returnExpr
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
    <|> EVar <$> ident
    <|> EInt <$> int
    <|> elist
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

{-}
caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  alts <-
    braces (many (caseAlt <* optional (symbol ";")))
      <|> many (caseAlt <* optional (symbol ";"))
  if null alts
    then empty -- "case expression requires at least one alternative"
    else return (ECase scrutinee alts)

caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  alts <-
    braces (sepEndBy1 caseAlt (skipMany (symbol ";" <|> void (token TokNewline))))
      <|> sepEndBy1 caseAlt (skipMany (symbol ";" <|> void (token TokNewline)))
  return (ECase scrutinee alts)
-}

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  skipMany newline
  alts <- caseBody
  return (ECase scrutinee alts)

caseBody :: Parser [CaseAlt]
caseBody =
  braces (sepEndBy1 caseAlt sep)
    <|> sepEndBy1 caseAlt sep
  where
    sep = skipMany (symbol ";" <|> void (token TokNewline))

{-}
caseAlt :: Parser CaseAlt
caseAlt = do
  alt <- try caseAltGuarded <|> caseAltSimple
  myTrace (">> caseAlt after alt")
  lookAhead caseAltEnd
  return alt
-}

caseAlt :: Parser CaseAlt
caseAlt = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< caseAlt next token: " ++ show t)
  myTrace ("<< caseAlt")
  pat <- pattern
  alt <-
    (CaseAltGuard pat <$> some caseGuard)
      <|> (CaseAlt pat <$> (token TokArrow *> expr))
  skipMany (symbol ";" <|> newline)
  return alt

newline :: Parser ()
newline = void (token TokNewline)

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

{-}
caseGuard :: Parser (Expr, Expr)
caseGuard = do
  myTrace ("<< caseGuard")
  symbol "|"
  cond <- expr
  t <- lookAhead anyToken
  myTrace ("in caseGuard expr next token: " ++ show t)
  token TokArrow
  body <- expr
  myTrace (">> caseGuard")
  return (cond, body)
-}

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

doExpr :: Parser Expr
doExpr = do
  keyword "do"
  myTrace "<< doExpr" >> pure ()
  symbol "{"
  stmts <- many stmt
  symbol "}"
  myTrace ">> doExpr" >> pure ()
  return (EDo stmts)

stmt :: Parser Stmt
stmt = do
  notFollowedBy (symbol "}")
  s <- try letStmt <|> try bindStmt <|> exprStmt
  optional (symbol ";") -- ← ここで吸収！
  return s

letStmt :: Parser Stmt
letStmt = do
  keyword "let"
  binds <- sepBy1 binding (symbol ";")
  return (LetStmt binds)

binding :: Parser (Pattern, Expr)
binding = do
  pat <- pattern
  symbol "="
  e <- expr -- ← ここが exprSeq だと壊れる可能性あり！
  return (pat, e)

bindStmt :: Parser Stmt
bindStmt = do
  pat <- pattern
  -- myTrace ">> bindStmt" >> pure ()
  symbol "<-"
  e <- expr -- expr
  myTrace ("<< bindStmt: " ++ show pat) >> pure (Bind pat e)

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

whereClause :: Parser [(Pattern, Expr)]
whereClause = do
  keyword "where"
  -- myTrace ">> whereClause" >> pure ()
  defs <- braces (sepBy def (symbol ";"))
  myTrace "<< whereClause" >> pure ()
  return defs

debugPeek :: Parser ()
debugPeek = do
  t <- peekToken
  Parser $ \tokens ->
    Just ((), tokens)

peekToken :: Parser Token
peekToken = Parser $ \tokens -> case tokens of
  [] -> Nothing
  (t : _) -> Just (t, tokens)

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

-- bracesCase :: Parser [(Pattern, Expr)]
-- bracesCase = braces (caseAlt `sepEndBy1` symbol ";")

-- plainCase :: Parser [(Pattern, Expr)]
-- plainCase = some caseAlt

elist :: Parser Expr
elist = brackets (try listComp <|> try range <|> list)

range :: Parser Expr
range = do
  start <- expr
  symbol ".."
  end <- expr
  return (ERange start end)

list :: Parser Expr
list = do
  elems <- expr `sepBy` symbol ","
  return (EList elems)

listComp :: Parser Expr
listComp = do
  body <- expr
  symbol "|"
  qualifiers <- qualifier `sepBy` symbol ","
  return (EListComp body qualifiers)

qualifier :: Parser Qualifier
qualifier = try generator <|> guardExpr

generator :: Parser Qualifier
generator = do
  var <- ident
  symbol "<-"
  val <- expr
  return (EGenerator var val)

guardExpr :: Parser Qualifier
guardExpr = EGuard <$> expr

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

choice1 :: Parser a -> Parser a -> Parser a
choice1 p q = Parser $ \input ->
  case runParser p input of
    Just r -> Just r
    Nothing -> runParser q input

choice :: [Parser a] -> Parser a
choice [] = empty
choice (p : ps) = choice1 p (choice ps)

operator :: Parser String
operator = choice (map (\s -> symbol s >> return s) allOps)
  where
    allOps = ["==", "/=", ">=", "<=", "+", "-", "*", "/", ">", "<"]

some1 :: Parser a -> Parser [a]
some1 p = do
  x <- p
  xs <- many p
  return (x : xs)
