{-# LANGUAGE LambdaCase #-}

module Expr.ExprParser
  ( expr,
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
import Debug.Trace (trace, traceIO, traceShowId)
import Expr.AST
import Expr.Combinator
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..))

toplevel :: Parser (String, Expr)
toplevel = do
  name <- traceShowId <$> ident -- ← ここで name を強制的に表示！
  args <- many ident
  symbol "="
  exprBody <- expr
  return (name, foldr ELam exprBody args)

expr :: Parser Expr
-- expr = doExpr <|> expr2 -- doExpr を先に置くと優先される
expr =
  try doExpr
    <|> binOpExpr

--  <|> try exprOthers

{-}
doExpr :: Parser Expr
doExpr = do
  keyword "do"
  stmts <- braces (stmt `sepBy1` symbol ";")
  return (EDo stmts)

doExpr :: Parser Expr
doExpr = do
  trace ">> doExpr" (pure ())
  keyword "do"
  stmts <- braces (stmt `sepBy1` symbol ";")
  trace ("<< doExpr: " ++ show stmts) (pure (EDo stmts))

stmt :: Parser Stmt
stmt = try bindStmt <|> exprStmt

bindStmt :: Parser Stmt
bindStmt = do
  pat <- pattern
  symbol "<-"
  e <- expr
  return (Bind pat e)

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr
-}
{-}
doExpr :: Parser Expr
doExpr = do
  trace ">> doExpr" (pure ())
  keyword "do"
  stmts <- braces (stmt `sepBy` symbol ";")
  trace ("<< doExpr: " ++ show stmts) (pure (EDo stmts))
-}

doExpr :: Parser Expr
doExpr = try $ do
  trace ">> doExpr" (pure ())
  keyword "do"
  symbol "{"
  stmts <- stmt `sepBy` symbol ";"
  symbol "}"
  return (EDo stmts)

{-}
stmt :: Parser Stmt
stmt = trace ">> stmt" (pure ()) *> (try bindStmt <|> exprStmt)
-}

stmt :: Parser Stmt
stmt = trace ">> stmt" (pure ()) *> (try letStmt <|> try bindStmt <|> exprStmt)

letStmt :: Parser Stmt
letStmt = do
  trace ">> letStmt" (pure ())
  keyword "let"
  defs <- def `sepBy1` symbol ";"
  trace ("<< letStmt: " ++ show defs) (pure (LetStmt defs))

-- return (LetStmt defs)

bindStmt :: Parser Stmt
bindStmt = do
  trace ">> bindStmt" (pure ())
  pat <- pattern
  symbol "<-"
  e <- expr
  trace ("<< bindStmt: " ++ show pat) (pure (Bind pat e))

exprStmt :: Parser Stmt
exprStmt = do
  trace ">> exprStmt" (pure ())
  e <- expr
  trace ("<< exprStmt: " ++ show e) (pure (ExprStmt e))

exprOthers :: Parser Expr
exprOthers = do
  trace ">> entering exprOthers" (pure ())
  -- _ <- pure $ trace ">> entering exprOthers" ()
  e <- binOpExpr
  mdefs <- optional whereClause
  case mdefs of
    Nothing -> return e
    Just defs -> trace "<< parsed whereClause" (return (ELet defs e))

{-}
binOpExpr :: Parser Expr
binOpExpr = exprLevel1

exprLevel1 :: Parser Expr
exprLevel1 = chainl1 exprLevel2 (binOp ["+"])

exprLevel2 :: Parser Expr
exprLevel2 = exprLevel3
-}
binOpExpr :: Parser Expr
binOpExpr = exprCmp

exprCmp :: Parser Expr
exprCmp = chainl1 exprLevel1 (binOp [">", "<", ">=", "<=", "==", "/="])

exprLevel1 :: Parser Expr
exprLevel1 = chainl1 exprLevel2 (binOp ["+", "-"])

exprLevel2 :: Parser Expr
exprLevel2 = chainl1 exprLevel3 (binOp ["*", "/"])

{-}
exprLevel3 :: Parser Expr
exprLevel3 = do
  _ <- pure $ trace ">> entering exprLevel3" ()
  try lambdaExpr
    <|> try letExpr
    <|> try ifExpr
    <|> try caseExpr
    <|> appExpr
exprLevel3 :: Parser Expr
exprLevel3 = do
  trace ">> entering exprLevel3" (pure ())
  try ifExpr
    <|> try letExpr
    <|> try lambdaExpr
    <|> try caseExpr
    <|> appExpr
-}

exprLevel3 :: Parser Expr
exprLevel3 = do
  trace ">> entering exprLevel3" (pure ())
  try doExpr
    <|> try ifExpr
    <|> try letExpr
    <|> try lambdaExpr
    <|> try caseExpr
    <|> appExpr

{-}
caseExpr :: Parser Expr
caseExpr = do
  trace ">> caseExpr" (pure ())
  keyword "case"
  scrutinee <- expr
  trace ">> after scrutinee" (pure ())
  keyword "of"
  alts <- braces (sepBy caseAlt (symbol ";"))
  return (ECase scrutinee alts)
-}

caseExpr :: Parser Expr
caseExpr = do
  trace ">> caseExpr" (pure ())
  keyword "case"
  scrutinee <- expr
  trace ">> after scrutinee" (pure ())
  keyword "of"
  alts <- braces (sepBy caseAlt (symbol ";")) <|> sepBy1 caseAlt (symbol ";")
  -- alts <- braces (some caseAlt) <|> some caseAlt
  return (ECase scrutinee alts)

whereClause :: Parser [(Pattern, Expr)]
whereClause = do
  trace ">> entering whereClause" (pure ())
  keyword "where"
  defs <- braces (sepBy def (symbol ";"))
  return defs

debugPeek :: Parser ()
debugPeek = do
  t <- peekToken
  Parser $ \tokens -> Just (trace ("Next token: " ++ show t) (), tokens)

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

{-}
letExpr :: Parser Expr
letExpr = do
  keyword "let"
  defs <- def `sepBy1` symbol ";"
  keyword "in"
  body <- expr
  return (ELet defs body)
-}

letExpr :: Parser Expr
letExpr = do
  trace ">> letExpr" (pure ())
  keyword "let"
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

{-}
ifExpr :: Parser Expr
ifExpr = do
  trace ">> ifExpr" (pure ())
  keyword "if"
  cond <- expr
  keyword "then"
  thenBranch <- expr
  keyword "else"
  elseBranch <- expr
  return (EIf cond thenBranch elseBranch)
-}
ifExpr :: Parser Expr
ifExpr = do
  trace ">> ifExpr" (pure ())
  keyword "if"
  cond <- binOpExpr
  keyword "then"
  thenBranch <- expr
  keyword "else"
  elseBranch <- expr
  return (EIf cond thenBranch elseBranch)

bracesCase :: Parser [(Pattern, Expr)]
bracesCase = braces (caseAlt `sepEndBy1` symbol ";")

plainCase :: Parser [(Pattern, Expr)]
plainCase = some caseAlt

caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  trace ">> caseAlt" (pure ())
  pat <- pattern
  token TokArrow
  trace ">> before body" (pure ())
  body <- exprLevel3
  -- body <- expr
  trace ("<< caseAlt done: " ++ show pat) (pure (pat, body))

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

atom :: Parser Expr
atom =
  try caseExpr
    <|> parens exprOrTuple
    <|> try elist
    <|> try (EVar <$> ident)
    <|> (EInt <$> int)

exprOrTuple :: Parser Expr
exprOrTuple = do
  es <- expr `sepBy1` symbol ","
  return $ case es of
    [single] -> single
    _ -> ETuple es

appExpr :: Parser Expr
appExpr = do
  f <- atom
  args <- many atom
  return (foldl EApp f args)

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \case
  TokOperator op | op `elem` ops -> Just (EBinOp op)
  _ -> Nothing
