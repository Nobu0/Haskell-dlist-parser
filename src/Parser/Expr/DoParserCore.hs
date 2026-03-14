{-# LANGUAGE LambdaCase #-}

module Parser.Expr.DoParserCore (doExprCore) where

import AST.Expr
import Control.Applicative (empty, many, optional, (<|>))
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore -- (exprCore)
import Parser.Expr.PatternParser
import Utils.MyTrace

doExprCore :: Parser Expr -> Parser Expr
doExprCore expr = do
  keyword "do"
  skipNL
  bracesV $ do
    stmts <- doBlock expr
    myTrace (">>*doExprCore: stmts " ++ show stmts)
    return (EDo stmts)

doBlock :: Parser Expr -> Parser [Stmt]
doBlock expr = do
  t <- optional (lookAhead anyToken)
  case t of
    Just (TokSymbol "}") -> pure [] -- 空の{}
    _ -> do
      f <- doStmt expr
      m <- many (doStmt expr)
      myTrace (">>*doBlock: (f:m)" ++ show (f : m))
      return (f : m)

doStmt :: Parser Expr -> Parser Stmt
doStmt expr = do
  rt <-
    try (bindStmt expr)
      <|> try (letStmt expr)
      <|> exprStmt expr
  myTrace (">>*doStmt: rt " ++ show rt)
  return rt

exprStmt :: Parser Expr -> Parser Stmt
exprStmt expr = do
  skipVNL
  e <- expr
  myTrace (">>*exprStmt " ++ show e)
  return (ExprStmt e)

bindStmt :: Parser Expr -> Parser Stmt
bindStmt expr = do
  myTrace ("<< bindStmt")
  -- まず、次のトークン列に "<-" が含まれるか確認
  skipVNL
  pat <- pattern
  symbol "<-"
  skipNL
  e <- expr
  myTrace (">>*bindStmt pat= " ++ show pat ++ " e= " ++ show e)
  return (Bind pat e)

letStmt :: Parser Expr -> Parser Stmt
letStmt expr = do
  skipVNL
  keyword "let"
  myTrace ("<< letStmt")
  binds <- bindings -- sepBy1 binding (symbol ";")
  myTrace (">>*letStmt: binds " ++ show binds)
  -- bracesV $ do ここはオプションだから改行の判断はできない。
  mIn <- optional (try (keyword "in"))
  case mIn of
    Just _ -> empty
    Nothing -> return (LetStmt binds)
  where
    bindings = do
      b <- binding
      myTrace ("<< letStmt: b " ++ show b)
      -- bracesV $ do
      bs <- many binding
      return (b : bs)
    binding = do
      skipNL
      pat <- pattern
      symbol "="
      -- skipNL
      e <- expr
      return (pat, e)

doSemi :: Parser ()
doSemi =
  skipMany1 (try (symbol ";") <|> newline)

-- symbol ";"