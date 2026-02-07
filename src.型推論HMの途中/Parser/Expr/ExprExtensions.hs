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
import Control.Applicative (many, optional, (<|>))
import Data.Functor (void)
-- (keyword) -- , whereClause)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.CaseParserCore (caseExprCore)
import Parser.Expr.DoParserCore (doExprCore)
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.ListParserCore (listExprCore)
import Parser.Expr.PatternParser (pattern)
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
        TokKeyword "let" -> letExpr
        TokKeyword "if" -> ifExpr
        TokKeyword "for" -> forExpr
        TokKeyword "return" -> returnExpr
        TokSymbol "[" -> listExprCore expr
        _ -> exprCore

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

bindings = do
  b <- binding
  bs <- many binding
  return (b : bs)

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

newline :: Parser ()
newline = void (token TokNewline)

skipMany1 :: Parser a -> Parser ()
skipMany1 p = p *> skipMany p

skipMany :: Parser a -> Parser ()
skipMany p = Parser $ \ts ->
  case runParser p ts of
    Just (_, ts') -> runParser (skipMany p) ts'
    Nothing -> Just ((), ts)

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

binding :: Parser Binding
-- binding = try valueBinding <|> funBinding
binding = try funBinding <|> valueBinding

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

skipNewlines :: Parser ()
skipNewlines = do
  _ <- many (tokenIs (\t -> if t == TokNewline then Just () else Nothing))
  return ()
