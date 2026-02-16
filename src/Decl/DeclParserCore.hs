{-# LANGUAGE LambdaCase #-}

module Decl.DeclParserCore where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (Constraint), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (intercalate)
-- ★ ここが正しい

-- (keyword) -- , whereClause)

import Decl.DeclParser.Data
import Decl.DeclParser.Fun
import Decl.DeclParser.Import
import Decl.DeclParser.Module
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern, patternParser)
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
isEOF :: Parser Bool
isEOF = Parser $ \ts ->
  case ts of
    [] -> Just (True, [])
    _ -> Just (False, ts)

decl :: Parser Decl
decl = do
  skipNewlines
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
  myTrace ("<< declBody: return " ++ show d)
  return d

declDispatch :: Parser Decl
declDispatch = do
  t <- lookAhead anyToken
  myTrace ("<< decl dispatch: " ++ show t)
  case t of
    TokKeyword "data" -> dataDecl
    TokKeyword "newType" -> newtypeDecl
    TokKeyword "import" -> importDecl
    TokKeyword "instance" -> instanceDecl
    TokKeyword "module" -> moduleDecl
    TokKeyword "class" -> classDecl
    TokKeyword "type" -> typeAliasDecl
    -- _ -> try funDecl <|> valueDecl
    TokIdent _ -> try (funDecl) <|> try typeSigDecl <|> valueDecl
    -- TokSymbol "{" -> try (braces (funDecl decl)) <|> empty
    TokSymbol "(" -> try typeSigDecl <|> empty
    _ -> do
      myTrace ("<< unknown token in decl: " ++ show t)
      empty

-- Haskell ファイル全体
program :: Parser [Decl]
program = many decl

typeSigDecl :: Parser Decl
typeSigDecl = do
  t <- lookAhead anyToken
  myTrace ("<< typeSigDecl: " ++ show t)
  name <- ident <|> operator -- name
  symbol "::"
  ty <- parseType
  myTrace ("<< parsed type signature: " ++ name ++ " :: " ++ show ty)
  let decl = DeclTypeSig name ty
  myTrace ("<< returning DeclTypeSig: " ++ show decl)
  return decl

-- 値宣言
valueDecl :: Parser Decl
valueDecl = do
  t <- lookAhead anyToken
  myTrace ("<< valueDecl: " ++ show t)
  pat <- patternParser
  symbol "="
  body <- expr
  return (DeclValue pat body)

-- コンストラクタ
constr :: Parser Constraint
constr = do
  myTrace "<< constr parser called"
  cname <- typeIdent
  tys <- many parseType
  return (Constraint cname tys)

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

instanceDecl :: Parser Decl
instanceDecl = do
  myTrace "<< instanceDecl parser called"
  keyword "instance"
  ctx <- optional (try (constraintList <* keyword "=>"))
  className <- typeIdent
  args <- some typeAtom
  keyword "where"
  methods <- bracedBlock decl
  return (DeclInstance ctx className args methods)

classDecl :: Parser Decl
classDecl = do
  myTrace "<< classDecl parser called"
  keyword "class"
  className <- typeIdent
  vars <- some ident
  keyword "where"
  t <- lookAhead anyToken
  methods <- bracedBlock decl
  return $ DeclClass className vars methods

typeAliasDecl :: Parser Decl
typeAliasDecl = do
  myTrace "<< typeAliasDecl parser called"
  keyword "type"
  name <- typeIdent
  vars <- many ident
  symbol "="
  body <- parseType
  return $ DeclTypeAlias name vars body
