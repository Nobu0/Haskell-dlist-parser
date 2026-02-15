{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ExprExtensions
  ( expr,
    exprTop,
    exprSeq,
    exprSep,
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
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.CaseParserCore (caseExprCore)
import Parser.Expr.DoParserCore (doExprCore)
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.ListParserCore (listExprCore)
import Parser.Expr.PatternParser (pattern)
import Parser.SQL.SQLParser
import Utils.MyTrace

expr :: Parser Expr
expr = do
  e <- exprDispatch
  postfix e
  where
    -- すべての構文の入口
    exprDispatch = do
      t <- lookAhead anyToken
      myTrace ("<< expr dispatch: " ++ show t)
      case t of
        TokKeyword "do" -> doExprCore expr
        TokKeyword "case" -> caseExprCore expr
        -- TokKeyword "let" -> (try letExpr <|> pLetExpr)
        TokKeyword "let" -> letBlock
        TokKeyword "if" -> ifExpr
        TokKeyword "for" -> forExpr
        TokKeyword "return" -> returnExpr
        TokKeyword "sql" -> parseSQL
        TokSymbol "[" -> listExprCore expr
        _ -> exprCore

letBlock :: Parser Expr
letBlock = do
  t <- lookAhead anyToken
  myTrace ("<< letBlock next token: " ++ show t)
  try letExpr <|> pLetExpr

-- letBlock = try pLetExpr <|> letExpr

{-}
-- 後置構文（where など）
postfix :: Expr -> Parser Expr
postfix e =
  try
    ( do
        skipNewlines
        -- t <- lookAhead anyToken
        -- myTrace ("<< postfix next token: " ++ show t)
        binds <- whereClause
        postfix (EWhere e binds)
    )
    <|> return e
-}

postfix :: Expr -> Parser Expr
postfix e = do
  skipNewlines
  mbBinds <- whereClause
  case mbBinds of
    Just binds -> postfix (EWhere e binds)
    Nothing -> return e

whereClause :: Parser (Maybe [Binding])
whereClause =
  try (keyword "where" >> bindings >>= \bs -> return (Just bs))
    <|> return Nothing

{-}
bindings = do
  b <- binding
  bs <- many binding
  return (b : bs)
-}
bindings :: Parser [Binding]
bindings = do
  skipNewlines
  b <- binding
  bs <- many (skipSeparators >> binding)
  return (b : bs)

-- bindings :: Parser [Binding]
-- bindings = some binding

binding :: Parser Binding
binding = try valueBinding <|> funBinding

{-}
binding :: Parser Binding
binding = do
  pat <- pattern
  _ <- symbol "="   -- ここで '=' が無ければ binding は失敗する
  val <- expr
  return (pat, val)
-}
-- ============================================
--  exprTop / exprSeq
-- ============================================

exprTop :: Parser Expr
exprTop = try exprSeq <|> expr

exprSeq :: Parser Expr
exprSeq = do
  es <- sepEndBy1 (try expr) exprSep
  return $ if length es == 1 then head es else ESeq es

exprSep :: Parser ()
exprSep = skipMany1 (symbol ";" <|> newline)

-- ============================================
--  let / if / return / for
-- ============================================
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
  th <- expr
  keyword "else"
  el <- expr
  return (EIf cond th el)

returnExpr :: Parser Expr
returnExpr = do
  keyword "return"
  e <- expr
  return (EReturn e)

forExpr :: Parser Expr
forExpr = do
  keyword "for"
  qs <- sepBy1 qualifier (symbol ",")
  token TokArrow
  body <- expr
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  keyword "in"
  src <- expr
  return (QGenerator pat src)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> expr

{-}
funDecl :: Parser Decl
funDecl = do
  name <- ident
  args <- many pattern
  symbol "="
  body <- expr
  return (FunDecl name args body)
-}

-- binding = try funBinding <|> valueBinding
{-}
funBinding :: Parser Binding
funBinding = do
  optional (newline)
  t <- lookAhead anyToken
  myTrace ("<< funBinding next token: " ++ show t)
  name <- ident
  args <- many pattern
  symbol "="
  body <- expr
  -- optional (newline)
  return (PApp (PVar name) args, body)
-}

funBinding :: Parser Binding
funBinding = do
  optional newline
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
  body <- expr
  return (PApp (PVar name) args, body)

valueBinding :: Parser Binding
valueBinding = do
  optional (newline)
  t <- lookAhead anyToken
  myTrace ("<< valueBinding next token: " ++ show t)
  pat <- pattern
  symbol "="
  body <- expr
  -- optional (newline)
  return (pat, body)

{-}
letExpr :: Parser Expr
letExpr = do
  keyword "let"
  -- myTrace ("<< letExpr")
  binds <- bindingsBlock
  optional (newline)
  mIn <- optional (keyword "in")
  optional (newline)
  case mIn of
    Just _ -> do
      body <- expr
      return (ELet binds body)
    Nothing ->
      return (ELet binds (EVar "__unit__"))
-}
letExpr :: Parser Expr
letExpr = do
  keyword "let"
  t <- lookAhead anyToken
  myTrace ("<< letExpr next token: " ++ show t)
  binds <- bindingsBlock
  optional newline
  bracesV3 $ do
    mIn <- optional (keyword "in")
    optional newline
    case mIn of
      Just _ -> do
        body <- expr
        return (ELetBlock binds body)
      Nothing ->
        if null binds
          then empty -- ← これが正しい
          else return (ELetBlock binds (EVar "__unit__"))

{-}
  case mIn of
    Just _ -> do
      body <- expr
      return (ELetBlock binds body)
    Nothing ->
      return (ELetBlock binds (EVar "__unit__"))
-}

pLetExpr :: Parser Expr
pLetExpr = do
  keyword "let"
  t <- lookAhead anyToken
  myTrace ("<< pLetExpr next token: " ++ show t)
  pat <- pattern
  symbol "="
  e1 <- expr
  bracesV3 $ do
    keyword "in"
    e2 <- expr
    return (ELet pat e1 e2)

bindingsBlock :: Parser [Binding]
bindingsBlock = do
  -- optional (token TokNewline)
  braces (sepBy binding (symbol ";"))
    <|> sepBy binding (symbol ";")

{-}
whereClause :: Parser [Binding]
whereClause = do
  optional (newline)
  t <- lookAhead anyToken
  myTrace ("<< whereClause next token: " ++ show t)
  keyword "where"
  bindingsBlock
-}
