{-# LANGUAGE LambdaCase #-}

module Parser.Expr.DoParserCore (doExprCore) where

import AST.Expr
import Control.Applicative (empty, many, optional, (<|>))
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.PatternParser
import Utils.MyTrace

doExprCore :: Parser Expr -> Parser Expr
doExprCore expr = do
  keyword "do"
  bracesV $ do
    -- stmts <- sepBy (doStmt expr) doSemi
    stmts <- doBlock expr
    myTrace ("<< doExprCore: " ++ show stmts)
    return (EDo stmts)

doBlock :: Parser Expr -> Parser [Stmt]
doBlock expr = do
  t <- lookAhead anyToken
  myTrace ("<< doBlock: " ++ show t)
  case t of
    TokSymbol "}" -> pure []
    _ -> do
      f <- doStmt expr
      m <- many (doStmt expr)
      return (f : m)

doStmt :: Parser Expr -> Parser Stmt
doStmt expr = do
  rt <-
    try (bindStmt expr)
      <|> try (letStmt expr)
      <|> exprStmt expr
  t <- lookAhead anyToken
  myTrace ("<< doStmt: next token " ++ show t ++ " rt " ++ show rt)
  return rt

exprStmt :: Parser Expr -> Parser Stmt
exprStmt expr = do
  e <- expr
  myTrace ("<< exprStmt " ++ show e)
  return (ExprStmt e)

{-}
bindStmt :: Parser Expr -> Parser Stmt
bindStmt expr = do
  pat <- pattern
  symbol "<-"
  e <- expr
  return (Bind pat e)
-}

bindStmt :: Parser Expr -> Parser Stmt
bindStmt expr = try $ do
  myTrace ("<< bindStmt")
  -- まず、次のトークン列に "<-" が含まれるか確認
  lookAhead $ do
    _ <- pattern
    symbol "<-"
    return ()
  -- 実際に読む
  pat <- pattern
  symbol "<-"
  e <- expr
  myTrace ("<< bindStmt pat= " ++ show pat ++ " e= " ++ show e)
  return (Bind pat e)

letStmt :: Parser Expr -> Parser Stmt
letStmt expr = do
  keyword "let"
  myTrace ("<< letStmt")
  binds <- bindings -- sepBy1 binding (symbol ";")
  mIn <- optional (try (keyword "in"))
  case mIn of
    Just _ -> empty
    Nothing -> return (LetStmt binds)
  where
    bindings = do
      b <- binding
      bs <- many (skipSeparators >> binding)
      return (b : bs)
    binding = do
      pat <- pattern
      symbol "="
      e <- expr
      return (pat, e)

doSemi :: Parser ()
doSemi =
  skipMany1 (try (symbol ";") <|> newline)

-- symbol ";"