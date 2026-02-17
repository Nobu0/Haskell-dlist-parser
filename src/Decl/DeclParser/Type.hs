{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Type (typeAliasDecl, newtypeDecl, constr) where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (Constraint), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
-- ★ ここが正しい

-- (keyword) -- , whereClause)

import Data.Char (isUpper)
import Data.List (intercalate)
import Decl.DeclParser.Util
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Type.TypeParser
import Utils.MyTrace

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

-- コンストラクタ
constr :: Parser Constraint
constr = do
  myTrace "<< constr parser called"
  cname <- typeIdent
  tys <- many parseType
  return (Constraint cname tys)

typeAliasDecl :: Parser Decl
typeAliasDecl = do
  myTrace "<< typeAliasDecl parser called"
  keyword "type"
  name <- typeIdent
  vars <- many ident
  symbol "="
  body <- parseType
  return $ DeclTypeAlias name vars body
