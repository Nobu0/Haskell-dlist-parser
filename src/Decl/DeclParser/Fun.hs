{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Fun (funDecl) where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (Constraint), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (intercalate)
-- ★ ここが正しい

-- (keyword) -- , whereClause)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern, patternParser)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

import Decl.DeclParser.Util

-- 関数宣言
funDecl :: Parser Decl -> Parser Decl
funDecl decl = do
  name <- ident
  args <- many patternParser
  t <- lookAhead anyToken
  case t of
    TokSymbol "=" -> parseSimple decl name args
    TokSymbol "|" -> parseGuarded2 decl name args
    TokSymbol "{" -> parseGuarded decl name args
    _ -> empty

parseSimple :: Parser Decl -> Name -> [Pattern] -> Parser Decl
parseSimple decl name args = do
  symbol "="
  e <- expr
  w <- optional (whereBlock decl)
  return (DeclFun name args Nothing (Just e) w)

parseGuarded :: Parser Decl -> Name -> [Pattern] -> Parser Decl
parseGuarded decl name args = do
  symbol "{"
  guards <- guardedRhs
  symbol "}"
  w <- optional (whereBlock decl)
  return (DeclFun name args (Just guards) Nothing w)

parseGuarded2 :: Parser Decl -> Name -> [Pattern] -> Parser Decl
parseGuarded2 decl name args = do
  guards <- guardedRhs2
  w <- optional (whereBlock decl)
  return (DeclFun name args (Just guards) Nothing w)

whereBlock :: Parser Decl -> Parser [Decl]
whereBlock decl = do
  keyword "where"
  symbol "{"
  decls <- many decl
  symbol "}"
  return decls

guardedRhs2 :: Parser [(Expr, Expr)]
guardedRhs2 = many guardedLine

guardedRhs :: Parser [(Expr, Expr)]
guardedRhs = sepBy1 guardedLine (symbol ";")

guardedLine :: Parser (Expr, Expr)
guardedLine = do
  t <- lookAhead anyToken
  myTrace ("<< guredLine: " ++ show t)
  symbol "|"
  cond <- expr
  symbol "="
  body <- expr
  return (cond, body)

funHead :: Parser (Name, [Pattern])
funHead = do
  p <- pattern
  myTrace ("<< funHead pattern: " ++ show p)
  case p of
    PVar name -> do
      args <- many pattern
      return (name, args)
    PApp (PVar name) args -> do
      moreArgs <- many pattern
      return (name, args ++ moreArgs)
    _ -> do
      myTrace "Function definition must start with a variable name"
      empty
