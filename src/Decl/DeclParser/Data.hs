{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Data (dataDecl) where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type
import Control.Applicative (empty, many, optional, some, (<|>))
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

dataDecl :: Parser Decl
dataDecl = do
  keyword "data"
  typeName <- identI
  typeVars <- many identI
  skipNL
  symbol "="
  skipNL
  constrs <- dataConstrBlock
  skipNL
  derivs <- option [] derivingClause
  return $ DeclData typeName typeVars constrs derivs

dataConstrBlock :: Parser [Constraint]
dataConstrBlock = do
  e <- dataConstr
  xs <- many dataConstr
  return (e : xs)

dataConstr :: Parser Constraint
dataConstr = do
  skipNL
  optional (symbol "|")
  e <- try dataConstrRecord <|> dataConstrNormal
  return e

dataConstrNormal :: Parser Constraint
dataConstrNormal = do
  skipNL
  name <- identI
  skipNL
  args <- many typeAtom
  myTrace ("<< dataConstrNormal: " ++ show name ++ " " ++ show args)
  return $ Constraint name args

dataConstrRecord :: Parser Constraint
dataConstrRecord = do
  skipNL
  name <- identI
  skipNL
  t <- lookAhead anyToken
  myTrace ("<< dataConstrRecord: " ++ show t)
  case t of
    TokSymbol "{" -> do
      fields <- fieldDefs
      myTrace ("<< dataConstrRecord: " ++ show name ++ " " ++ show fields)
      return $ ConstraintRecord name fields
    _ -> empty

fieldDef :: Parser Field
fieldDef = do
  name <- identI
  symbol "::"
  ty <- typeExpr
  return $ Field name ty

fieldDefs :: Parser [Field]
fieldDefs = do
  skipNL
  symbol "{"
  skipNL
  fields <- sepBy1Skip fieldDef (symbol ",")
  -- fields <- getTypeDefs []
  skipNL
  symbol "}"
  return (reverse fields)

getTypeDefs :: [Field] -> Parser [Field]
getTypeDefs acc = do
  t <- lookAhead anyToken
  myTrace ("<< typeDef:1 next token " ++ show t)
  case t of
    TokSymbol "}" -> do
      return acc
    _ -> do
      name <- identI
      symbol "::"
      ty <- typeExpr
      skipNL
      t <- lookAhead anyToken
      myTrace ("<< typeDef:2 next token " ++ show t ++ " " ++ show name ++ " " ++ show ty)
      case t of
        TokSymbol "," -> do
          symbol ","
          skipNL
          -- optional (symbol ";")
          getTypeDefs (Field name ty : acc)
        TokSymbol ";" -> do
          symbol ";"
          getTypeDefs (Field name ty : acc)
        TokSymbol "}" -> do
          return (Field name ty : acc)
        _ -> empty

derivingClause :: Parser [Name]
derivingClause = do
  keyword "deriving"
  parens (sepBy1 typeIdent (symbol ",")) <|> fmap pure typeIdent
