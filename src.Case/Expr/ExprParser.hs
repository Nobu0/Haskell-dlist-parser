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

{-}
toplevel :: Parser (String, Expr)
toplevel = do
  _ <- pure $ trace ">> entering toplevel" ()
  name <- ident
  args <- many ident
  symbol "="
  exprBody <- expr
  trace ("<< parsed toplevel: " ++ name) (return (name, foldr ELam exprBody args))
-}

expr :: Parser Expr
expr = do
  _ <- pure $ trace ">> entering expr" ()
  e <- binOpExpr
  mdefs <- optional whereClause
  case mdefs of
    Nothing -> return e
    Just defs -> trace "<< parsed whereClause" (return (ELet defs e))

binOpExpr :: Parser Expr
binOpExpr = exprLevel1

exprLevel1 :: Parser Expr
exprLevel1 = chainl1 exprLevel2 (binOp ["+"])

exprLevel2 :: Parser Expr
exprLevel2 = exprLevel3

exprLevel3 :: Parser Expr
exprLevel3 = do
  _ <- pure $ trace ">> entering exprLevel3" ()
  try lambdaExpr
    <|> try letExpr
    <|> try ifExpr
    <|> try caseExpr
    <|> appExpr

{-}
caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrutinee <- trace ">> parsing scrutinee" binOpExpr
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
  alts <- braces (sepBy caseAlt (symbol ";"))
  return (ECase scrutinee alts)

{-}
caseExpr :: Parser Expr
caseExpr = do
  _ <- pure $ trace ">> entering caseExpr" ()
  keyword "case"
  scrutinee <- binOpExpr
  keyword "of"
  alts <- braces (sepBy caseAlt (symbol ";"))
  return (ECase scrutinee alts)
-}

whereClause :: Parser [(Pattern, Expr)]
whereClause = do
  _ <- pure $ trace ">> entering whereClause" ()
  keyword "where"
  defs <- braces (sepBy def (symbol ";"))
  return defs

debugPeek :: Parser ()
debugPeek = do
  t <- peekToken
  Parser $ \tokens -> Just (trace ("Next token: " ++ show t) (), tokens)

{-}
debugPeek :: Parser ()
debugPeek = do
  t <- peekToken
  Parser $ \tokens -> Just (tokens, trace ("Next token: " ++ show t) ())

debugPeek :: Parser ()
debugPeek = do
  t <- peekToken
  Parser $ \tokens ->
    trace ("Next token: " ++ show t) $
      Just (tokens, ())
-}

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
  defs <- def `sepBy1` symbol ";"
  keyword "in"
  body <- expr
  return (ELet defs body)

{-}
def :: Parser (String, Expr)
def = do
  name <- ident
  symbol "="
  val <- expr
  return (name, val)
-}

def :: Parser (Pattern, Expr)
def = do
  p <- pattern
  symbol "="
  e <- expr
  return (p, e)

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  cond <- expr
  keyword "then"
  thenBranch <- expr
  keyword "else"
  elseBranch <- expr
  return (EIf cond thenBranch elseBranch)

bracesCase :: Parser [(Pattern, Expr)]
bracesCase = braces (caseAlt `sepEndBy1` symbol ";")

plainCase :: Parser [(Pattern, Expr)]
plainCase = some caseAlt

{-}
caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  pat <- pattern
  token TokArrow
  body <- expr
  _ <- optional (symbol ";")
  return (pat, body)
-}

caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  trace ">> caseAlt" (pure ())
  pat <- pattern
  token TokArrow
  body <- expr
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

-- 関数適用と演算子
{-}
atom :: Parser Expr
atom =
  parens exprOrTuple
    <|> try elist
    <|> (EVar <$> ident)
    <|> (EInt <$> int)
atom :: Parser Expr
atom =
  parens exprOrTuple
    <|> try elist
    <|> try (EVar <$> ident)
    <|> (EInt <$> int)
-}

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

{-
binOpExpr :: Parser Expr
binOpExpr = exprLevel1

exprLevel1 :: Parser Expr
exprLevel1 = chainl1 exprLevel2 (binOp ["+", "-"])

exprLevel2 :: Parser Expr
exprLevel2 = chainl1 exprLevel3 (binOp ["*", "/"])

exprLevel3 :: Parser Expr
exprLevel3 =
  lambdaExpr
    <|> letExpr
    <|> ifExpr
    <|> caseExpr
    <|> appExpr

exprLevel3 :: Parser Expr
exprLevel3 =
  do
    let _ = trace ">> entering exprLevel3" ()
    try lambdaExpr
    <|> try letExpr
    <|> try ifExpr
    <|> try caseExpr
    <|> appExpr
-}

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \case
  TokOperator op | op `elem` ops -> Just (EBinOp op)
  _ -> Nothing
