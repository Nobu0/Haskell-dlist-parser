{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Type
  ( typeDecl,
    newtypeDecl,
    constr,
    typeSigDecl,
    -- classDecl,
    -- instanceDecl,
  )
where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (..), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
-- ★ ここが正しい

-- (keyword) -- , whereClause)

import Data.Char (isUpper)
import Data.List (intercalate)
import Data.Map (keys)
import Decl.DeclParser.Util
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Type.TypeParser
import Utils.MyTrace


typeSigDecl :: Parser Decl
typeSigDecl = do
  t <- lookAhead anyToken
  myTrace ("<< typeSigDecl: " ++ show t)
  name <-
    try ident
      <|> do
        op <- parens operatorI
        return $ "(" ++ op ++ ")"
  t <- lookAhead anyToken
  myTrace ("<< typeSigDecl:2 " ++ show t)
  symbol "::"
  ty <- parseType
  myTrace ("<< parsed type signature: " ++ name ++ " :: " ++ show ty)
  let decl = DeclTypeSig name ty
  myTrace ("<< returning DeclTypeSig: " ++ show decl)
  return decl

-- newtype 宣言
newtypeDecl :: Parser Decl
newtypeDecl = do
  myTrace "<< newtypeDecl parser called"
  keyword "newtype"
  name <- typeIdent
  vars <- many identI
  myTrace("<< newtypeDecl name "++ show name++ " vars "++ show vars)
  symbol "="
  c <- constr
  myTrace(">>*newtypeDecl c "++ show c)
  return (DeclNewtype name vars c)

{-}
-- コンストラクタ
constr :: Parser Constraint
constr = do
  myTrace "<< constr parser called"
  cname <- typeIdent
  tys <- braces $ sepBy1 typeSig (symbol ",")
  return (Constraint cname tys)
-}

-- type Foo a b = (a, b)
typeDecl :: Parser Decl
typeDecl = do
  keyword "type"
  name <- identI
  vars <- many identI
  myTrace("<< typeDecl name "++ show name++ " vars "++ show vars)
  symbol "="
  typ <- typeExpr
  myTrace(">>*typeDecl typ "++ show typ)
  return $ DeclTypeAlias name vars typ

{-}
-- class Eq a where ...
classDecl :: Parser Decl
classDecl = do
  keyword "class"
  clsName <- identI
  vars <- many identI
  optional (TokKeyword "where")
  decls <- braces (many decl)
  return $ DeclClass clsName vars decls

-- instance (Show a) => Eq (Maybe a) where ...
instanceDecl :: Parser Decl
instanceDecl = do
  keyword "instance"
  ctx <- optional contextListParser -- Maybe [Constraint]
  clsName <- identI
  types <- many typeParser
  optional (keyword "where")
  decls <- braces (many decl)
  return $ DeclInstance ctx clsName types decls
-}

-- ident :: Parser String
-- ident = typeIdent <$> identI

contextListParser :: Parser [Constraint]
contextListParser = parens (sepBy1 constraintParser (symbol ","))

constraintParser :: Parser Constraint
constraintParser = do
  cls <- identI
  ty <- typeExpr
  return $ Constraint cls [ty]
