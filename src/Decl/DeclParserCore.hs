{-# LANGUAGE LambdaCase #-}

module Decl.DeclParserCore where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (..), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (intercalate)
-- ★ ここが正しい

-- (keyword) -- , whereClause)

import Decl.DeclParser.Data
import Decl.DeclParser.Fun
import Decl.DeclParser.Import
import Decl.DeclParser.Module
import Decl.DeclParser.Type
import Decl.DeclParser.Util
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Core.TokenParser (operator)
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

-- decls :: Parser [Decl]
-- decls = many decl
{-}
decl :: Parser Decl
decl = do
  skipNewlines
  myTrace "<< decl parser called"
  declBody
-}

decl :: Parser Decl
decl = do
  t <- lookAhead anyToken
  myTrace ("<< decl next token: " ++ show t)
  eof <- isEOF
  if eof
    then Parser $ \_ -> Nothing -- many decl に「もう終わり」と伝える
    else do
      myTrace "<< decl parser called"
      declBody

declBody :: Parser Decl
declBody = do
  d <- declDispatch
  optional (newline)
  myTrace (">>*declBody: e " ++ show d)
  return d

declDispatch :: Parser Decl
declDispatch = do
  skipNewlines
  t <- lookAhead anyToken
  myTrace ("<< decl dispatch: " ++ show t)
  case t of
    TokKeyword "data" -> dataDecl
    TokKeyword "newtype" -> newtypeDecl
    TokKeyword "import" -> importDecl
    TokKeyword "instance" -> instanceDecl
    TokKeyword "module" -> moduleDecl
    TokKeyword "class" -> classDecl
    TokKeyword "type" -> typeDecl
    TokLambdaCase -> empty
    -- _ -> try funDecl <|> valueDecl
    TokIdent _ -> try funDecl <|> try typeSigDecl <|> valueDecl
    -- TokSymbol "{" -> try (braces (funDecl decl)) <|> empty
    TokSymbol "(" -> try typeSigDecl
    -- TokVRBrace -> empty
    _ -> do
      myTrace ("<< unknown token in decl: " ++ show t)
      empty

-- Haskell ファイル全体
program :: Parser [Decl]
program = many decl

-- 値宣言
valueDecl :: Parser Decl
valueDecl = do
  t <- lookAhead anyToken
  myTrace ("<< valueDecl: " ++ show t)
  pat <- pattern -- patternParser
  symbol "="
  body <- expr
  return (DeclValue pat body)

instanceDecl :: Parser Decl
instanceDecl = do
  myTrace "<< instanceDecl parser called"
  keyword "instance"
  ctx <- optional (try (constraintList <* keyword "=>"))
  className <- typeIdent
  args <- some typeAtom
  keyword "where"
  bracesV $ do
    methods <- declMany
    myTrace $ ">>*instanceDecl: methods "++ show methods
    return (DeclInstance ctx className args methods)

declMany:: Parser [Decl]
declMany = do
  f <- decl
  optional (symbol ";")
  xs <- do 
    many $ do
      r <- decl
      optional (symbol ";")
      return r
  return (f : xs)

classDecl :: Parser Decl
classDecl = do
  myTrace "<< classDecl parser called"
  keyword "class"
  className <- typeIdent
  vars <- some ident
  keyword "where"
  bracesV $ do
    -- t <- lookAhead anyToken
    methods <- many decl -- (symbol ";")
    myTrace $ ">>*classDecl: methods "++ show methods
    return $ DeclClass className vars methods
