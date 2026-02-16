{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Import (importDecl) where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (Constraint), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (intercalate)
-- ★ ここが正しい

-- (keyword) -- , whereClause)

import Decl.DeclParser.Util
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern, patternParser)
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
  parens $
    pure ImportAllItems <$ symbol ".."
      <|> sepBy1 importIdent (symbol ",")

{-}
importIdent :: Parser ImportItem
importIdent = do
  name <- identI
  m <-
    optional $
      parensI $
        (ImportTypeAll name <$ symbol "..")
          <|> (ImportTypeSome name <$> sepBy1 identI (symbol ","))
  return $ case m of
    Just x -> x
    Nothing -> ImportVar name
-}
importIdent :: Parser ImportItem
importIdent = do
  name <- identI <|> parens operatorI
  m <-
    optional $
      parensI $
        (ImportTypeAll name <$ symbol "..")
          <|> (ImportTypeSome name <$> sepBy1 identI (symbol ","))
  return $ case m of
    Just x -> x
    Nothing -> ImportVar name

{-}
operatorI :: Parser Name
operatorI = do
  TokOperator op <- token satisfyOperator
  return op

satisfyOperator :: Token -> Bool
satisfyOperator (TokOperator _) = True
satisfyOperator _ = False
-}
