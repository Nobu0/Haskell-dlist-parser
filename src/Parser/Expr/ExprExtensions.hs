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
import Parser.Expr.ExprCore (exprCore)
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
  -- e <- eof
  -- myTrace ("<< expr:2 " ++ show rt ++ " " ++ show e)
  return rt

infixExpr :: Parser Expr
infixExpr = chainl1 exprTerm infixOp

exprTerm :: Parser Expr
exprTerm = do
  t <- lookAhead anyToken
  myTrace ("<< exprTerm: next token " ++ show t)
  e <- exprNoLoop
  postfix e

{-}
  o <- isEOF
  --x <- eof
  myTrace ("<< exprTerm: isEOF: " ++ show o ++ " e: " ++ show e)
  case o of
    True -> return e
    _ -> postfix e
-}

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
    -- TokKeyword "case" -> caseExpr
    TokLambdaCase -> lambdaCaseExpr exprNoLoop
    -- TokSymbol ";" -> do
    -- skipSeparators
    --  empty
    _ -> exprCore

{-}
infixExpr :: Parser Expr
infixExpr = chainl1 atomExpr infixOp

infixOp :: Parser (Expr -> Expr -> Expr)
infixOp = do
  op <- operatorI
  return (\a b -> EApp (EApp (EVar op) a) b)
exprNoInfix :: Parser Expr
exprNoInfix = do
  e <- exprDispatch
  postfix e

-}

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
{-}
postfix :: Expr -> Parser Expr
postfix e = do
  skipNewlines
  mbBinds <- whereClause
  case mbBinds of
    Just binds -> postfix (EWhere e binds)
    Nothing -> return e
-}
{-}
postfix :: Expr -> Parser Expr
postfix e = do
  -- skipNewlines
  -- 中置演算子を処理
  rest <- 
    optional
      ( do
          op <- operatorA -- operatorI
          myTrace ("<< postfix: infix operator = " ++ show op)
          rhs <- exprNoLoop
          return $ EApp (EApp (EVar op) e) rhs
      )
  myTrace ("<< postfix: rest = " ++ show rest ++ " expr = "++ show e)
  case rest of
    Just e' -> postfix e'
    Nothing -> do
      mbBinds <- whereClause
      case mbBinds of
        Just binds -> postfix (EWhere e binds)
        Nothing -> return e
-}

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
operatorAA :: Parser (Maybe String)
operatorAA = do
  t <- lookAhead anyToken
  case t of
    TokOperator s -> do
      _ <- anyToken
      return (Just s)
    _ -> return Nothing

postfix :: Expr -> Parser Expr
postfix e = do
  mop <- operatorAA
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
-}

{-}
infixApp :: Expr -> Parser Expr
infixApp lhs = do
  op <- operatorI
  rhs <- expr
  return $ EApp (EApp (EVar op) lhs) rhs
-}

whereClause :: Parser (Maybe [Binding])
whereClause = do
  t <- lookAhead anyToken
  myTrace ("<< whereClause: next token " ++ show t)
  try (keyword "where" >> bindings >>= \bs -> return (Just bs))
    <|> return Nothing

bindings :: Parser [Binding]
bindings = do
  bracesV $ do
    b <- binding
    bs <- many (skipSeparators >> binding)
    -- bs <- many (binding)
    -- bs <- sepEndBy binding (symbol ";") --exprSep
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
  e <- exprNoLoop
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
  -- optional (newline)
  t <- lookAhead anyToken
  myTrace ("<< valueBinding: next token: " ++ show t)
  pat <- pattern
  symbol "="
  t2 <- lookAhead anyToken
  myTrace ("<< valueBinding:2 next token: " ++ show t2)
  body <- exprNoLoop
  t3 <- lookAhead anyToken
  myTrace ("<< valueBinding:3 next token: " ++ show t3++ " body "++ show body)
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
