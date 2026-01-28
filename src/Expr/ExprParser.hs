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
exprTop = exprSeq <|> expr

expr :: Parser Expr
expr =
  try doExpr
    <|> try caseExpr
    <|> try binOpExpr
    <|> exprAtomic -- exprSeq

-- exprLevel3: 通常の式（再帰あり）
exprLevel3 :: Parser Expr
exprLevel3 = do
  try returnExpr
    <|> try doExpr
    <|> try ifExpr
    <|> try letExpr
    <|> try lambdaExpr
    <|> try caseExpr
    <|> appExpr

-- exprAtomic: exprSeq 専用の「再帰しない」式
exprAtomic :: Parser Expr
exprAtomic =
  try returnExpr
    <|> try doExpr
    <|> try ifExpr
    <|> try letExpr
    <|> try lambdaExpr
    <|> try caseExpr
    <|> appExpr

-- ここがparensエリア --
atom :: Parser Expr
atom =
  try pRecordExpr
    <|> try pRecordUpdate
    <|> try pOpSection
    <|> parens exprOrTuple
    <|> try elist
    <|> try (EVar <$> ident)
    <|> (EInt <$> int)

appExpr :: Parser Expr
appExpr = chainl1 atom (pure EApp)

exprSeq :: Parser Expr
exprSeq = do
  myTrace ">> exprSeq"
  es <- sepEndBy1 (try exprAtomic) (symbol ";")
  return $ if length es == 1 then head es else ESeq es

anyToken :: Parser Token
anyToken = Parser $ \input ->
  case input of
    (t : ts) -> Just (t, ts)
    [] -> Nothing

lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \input ->
  case p input of
    Just (a, _) -> Just (a, input) -- 結果はそのまま、入力は消費しない
    Nothing -> Nothing

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

some1 :: Parser a -> Parser [a]
some1 p = do
  x <- p
  xs <- many p
  return (x : xs)

bindStmt :: Parser Stmt
bindStmt = do
  pat <- pattern
  -- myTrace ">> bindStmt" >> pure ()
  symbol "<-"
  e <- exprAtomic -- expr
  myTrace ("<< bindStmt: " ++ show pat) >> pure (Bind pat e)

exprStmt :: Parser Stmt
exprStmt = do
  e <- exprAtomic
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

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \input ->
  case runParser p input of
    Nothing -> Just ((), input) -- p が失敗 → 成功
    Just _ -> Nothing -- p が成功 → 失敗

exprCmp :: Parser Expr
exprCmp = chainl1 exprLevel1 (binOp [">", "<", ">=", "<=", "==", "/="])

exprLevel1 :: Parser Expr
exprLevel1 = chainl1 exprLevel2 (binOp ["+", "-"])

exprLevel2 :: Parser Expr
exprLevel2 = chainl1 exprLevel3 (binOp ["*", "/"])

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  alts <-
    braces (some (caseAlt <* optional (symbol ";")))
      <|> some (caseAlt <* optional (symbol ";"))
  myTrace "<< caseExpr" >> pure ()
  return (ECase scrutinee alts)

caseAltWithSemi :: Parser (Pattern, Expr)
caseAltWithSemi = do
  alt <- caseAlt
  optional (symbol ";")
  return alt

caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  pat <- pattern
  token TokArrow
  -- t <- lookAhead anyToken
  -- myTrace ("next token: " ++ show t)
  -- myTrace "caseAlt" >> pure ()
  body <- exprSeq -- <* optional (symbol ";")
  return (pat, body)

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
    -- Just (myTrace ("Next token: " ++ show t) (), tokens)
    -- myTrace
    --  ("Next token: " ++ show t)
    --  >> pure
    --    ()
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

bracesCase :: Parser [(Pattern, Expr)]
bracesCase = braces (caseAlt `sepEndBy1` symbol ";")

plainCase :: Parser [(Pattern, Expr)]
plainCase = some caseAlt

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
  t <- lookAhead anyToken
  myTrace ("returnExpr next token: " ++ show t)
  keyword "return"
  -- myTrace "<< returnExpr key" >> pure ()
  notFollowedBy (symbol "_") -- ← これで `return _` を防ぐ
  e <- exprAtomic
  myTrace "<< returnExpr" >> pure ()
  return (EReturn e)

exprOrTuple :: Parser Expr
exprOrTuple = do
  es <- expr `sepBy1` symbol ","
  -- es <- exprAtomic `sepBy1` symbol ","
  return $ case es of
    [single] -> single
    _ -> ETuple es

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \case
  TokOperator op | op `elem` ops -> Just (EBinOp op)
  _ -> Nothing

exprAtomic2 :: Parser Expr
exprAtomic2 =
  --  try returnExpr
  try doExpr
    <|> try ifExpr
    <|> try letExpr
    <|> try lambdaExpr
    <|> try caseExpr
    <|> appExpr

exprAtomicP :: Parser Expr
exprAtomicP = do
  f <- atomBase
  args <- many atomBase
  return (foldl EApp f args)

atomBase :: Parser Expr
atomBase =
  try (EVar <$> ident)
    <|> parens expr

pRecord :: Parser [(String, Expr)]
pRecord = braces (sepBy field (symbol ","))

pRecordExpr :: Parser Expr
pRecordExpr = ERecord <$> pRecord

pRecordUpdate :: Parser Expr
pRecordUpdate = do
  base <- atomBase
  updates <- braces (sepBy field (symbol ","))
  return (ERecordUpdate base updates)

pOpSection :: Parser Expr
pOpSection =
  parens $
    try
      ( do
          e <- exprAtomic2
          op <- operator
          return (EOpSectionL op e)
      )
      <|> try
        ( do
            op <- operator
            e <- exprAtomic2
            return (EOpSectionR e op)
        )

field :: Parser (String, Expr)
field = do
  name <- ident
  symbol "="
  val <- expr -- exprAtomicP
  return (name, val)

operator :: Parser String
operator = choice (map (\s -> symbol s >> return s) allOps)
  where
    allOps = ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "/="]

choice1 :: Parser a -> Parser a -> Parser a
choice1 p q = Parser $ \input ->
  case runParser p input of
    Just r -> Just r
    Nothing -> runParser q input

choice :: [Parser a] -> Parser a
choice [] = empty
choice (p : ps) = choice1 p (choice ps)
