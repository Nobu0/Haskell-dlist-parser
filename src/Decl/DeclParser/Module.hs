{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Module (moduleDecl) where

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
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern, patternParser)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

moduleDecl :: Parser Decl
moduleDecl = do
  keyword "module"
  name <- moduleName
  try (exportWhere name) <|> exportWhere2 name

exportWhere :: String -> Parser Decl
exportWhere name = do
  exports <- optional (bracesV3 exportList)
  skipSeparators
  keyword "where"
  return (DeclModule name exports)

exportWhere2 :: String -> Parser Decl
exportWhere2 name = do
  bracesV $ do
    exports <- optional exportList
    skipSeparators
    keyword "where"
    return (DeclModule name exports)

exportItem :: Parser Export
exportItem = do
  t <- lookAhead anyToken
  myTrace ("<< exportItem: next token=" ++ show t)
  name <- typeIdent <|> ident <|> parens operatorI
  hasAll <- optional (parens (symbol ".."))
  return $ case hasAll of
    Just _ -> ExportType name True
    Nothing ->
      if isUpper (head name)
        then ExportType name False
        else ExportVar name

exportList :: Parser [Export]
exportList = parens (exportItem `sepEndBy` symbol ",")
