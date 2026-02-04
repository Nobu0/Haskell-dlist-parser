{-# LANGUAGE LambdaCase #-}

module Decl.DeclParserCore where

import Control.Applicative (many, optional, (<|>))
import Data.List (intercalate)

import AST.Decl
import AST.Module (Name)
import AST.Type (Type)
import AST.Pattern (Pattern(..))

import Parser.Core.Combinator
import Parser.Core.TokenParser

import Parser.Expr.PatternParser (patternParser)
import Parser.Expr.ExprExtensions (expr) -- ★ ここが正しい 
import Parser.Type.TypeParser (parseType, typeIdent)
import Utils.MyTrace
import Parser.Core.TokenParser -- (keyword) -- , whereClause)
import Lexer.Lexer (Token (..))

-- 1つの Decl をパース
{-}
decl :: Parser Decl
decl = do
  myTrace "<< decl parser called"
  try funDecl
    <|> try valueDecl
    <|> try dataDecl
    <|> try newtypeDecl
    <|> try importDecl
    <|> moduleDecl
-}
decl :: Parser Decl
decl = do
  myTrace "<< decl parser called"
  let declDispatch = do
        t <- lookAhead anyToken
        myTrace ("<< decl dispatch: " ++ show t)
        case t of
          TokKeyword "data"   -> dataDecl
          TokKeyword "newType" -> newtypeDecl
          TokKeyword "import" -> importDecl
          TokKeyword "module" -> moduleDecl
          _ -> try funDecl <|> valueDecl

  declDispatch


-- Haskell ファイル全体
program :: Parser [Decl]
program = many decl

-- 関数宣言
funDecl :: Parser Decl
funDecl = do
  myTrace "<< funDecl parser called"
  name <- ident
  args <- many patternParser
  t <- lookAhead anyToken
  myTrace ("<< funcdecl 2: " ++ show t)
  symbol "="
  body <- expr
  return (DeclFun (PConstr name args) body)

-- 値宣言
valueDecl :: Parser Decl
valueDecl = do
  myTrace "<< valueDecl parser called"
  pat <- patternParser
  symbol "="
  body <- expr
  return (DeclValue pat body)

-- import 文
importDecl :: Parser Decl
importDecl = do
  myTrace "<< importDecl parser called"
  keyword "import"
  modName <- moduleName
  return (DeclImport modName)

moduleName :: Parser Name
moduleName = intercalate "." <$> sepBy1 ident (symbol ".")

-- data 宣言
dataDecl :: Parser Decl
dataDecl = do
  myTrace "<< dataDecl parser called"
  keyword "data"
  name <- typeIdent
  vars <- many typeIdent
  symbol "="
  constrs <- constr `sepBy1` symbol "|"
  return (DeclData name vars constrs)

-- コンストラクタ
constr :: Parser Constr
constr = do
  myTrace "<< constr parser called"
  cname <- typeIdent
  tys <- many parseType
  return (Constr cname tys)

-- newtype 宣言
newtypeDecl :: Parser Decl
newtypeDecl = do
  myTrace "<< newtypeDecl parser called"
  keyword "newtype"
  name <- typeIdent
  vars <- many typeIdent
  symbol "="
  c <- constr
  return (DeclNewtype name vars c)

moduleDecl :: Parser Decl
moduleDecl = do
  keyword "module"
  name <- typeIdent -- <|> token TokTypeIdent)
  keyword "where"
  return (DeclModule name)
