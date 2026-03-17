{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Import (importDecl) where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (..), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (intercalate)
-- ★ ここが正しい

-- (keyword) -- , whereClause)

import Decl.DeclParser.Util
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

-- import 文
importDecl :: Parser Decl
importDecl = do
  myTrace "<< importDecl parser called"
  _ <- keyword "import"
  isQual <- option False (True <$ keyword "qualified")
  t <- lookAhead anyToken
  myTrace ("<< importDecl: " ++ show t)
  modName <- moduleName
  alias <- optional (keyword "as" *> identI)
  isHiding <- option False (True <$ keyword "hiding")
  items <- optional importList
  return $ DeclImport isQual modName alias isHiding items

importList :: Parser [ImportItem]
importList =
  parens $ pure ImportAllItems <$ symbol ".." <|> list
  where
    list = do
      f <- importIdent
      xs <- many importIdent
      return (f : xs)

importIdent :: Parser ImportItem
importIdent = do
  t <- lookAhead anyToken
  myTrace ("<< importIdent: next token " ++ show t)
  name <-
    try identI
      <|> operatorEdName
      <|> operatorIAsName
  m <- optional $ importType name
  optional (symbol ",")
  myTrace ("<< importIdent: m " ++ show m)
  return $ case m of
    Just x -> x
    Nothing -> ImportVar name

importType :: String -> Parser ImportItem
importType name =
  parensI (ImportTypeAll name <$ symbol "..") <|> (ImportTypeSome name <$> getNameList)

getNameList :: Parser [String]
getNameList = do
  list
  where
    name = do
      t <- lookAhead anyToken
      myTrace ("<< getNameList: next token " ++ show t)
      nm <- try identI <|> parens operatorIAsName <|> operatorEdName
      myTrace ("<< getNameList:2 next token " ++ show t ++ " " ++ show nm)
      optional (symbol ",")
      return nm
    list = do
      f <- name
      xs <- many1 name
      return (f : xs)
