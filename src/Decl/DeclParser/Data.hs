{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Data (dataDecl) where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type -- (Constraint (Constraint), Type (..))
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
  bracesV $ do
    symbol "="
    constrs <- sepBy1 dataConstr (symbol "|")
    derivs <- option [] derivingClause
    skipSeparators
    return $ DeclData typeName typeVars constrs derivs

dataConstr :: Parser Constraint
dataConstr = try dataConstrRecord <|> dataConstrNormal

dataConstrNormal :: Parser Constraint
dataConstrNormal = do
  name <- identI
  args <- many typeAtom
  myTrace ("<< dataConstrNormal: " ++ show name ++ " " ++ show args)
  optional (symbol ";")
  return $ Constraint name args

dataConstrRecord :: Parser Constraint
dataConstrRecord = do
  name <- identI
  skipBlk
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
  -- t <- lookAhead anyToken
  -- myTrace ("<< fieldDef: " ++ show t)
  name <- identI
  symbol "::"
  ty <- typeExpr
  -- t <- lookAhead anyToken
  -- myTrace ("<< fieldDef:2 " ++ show name ++ " " ++ show ty ++ " " ++ show t)
  return $ Field name ty

fieldDefs :: Parser [Field]
fieldDefs = do
  symbol "{"
  skipBlk
  fields <- sepBy1Skip fieldDef (symbol ",")
  -- fields <- getTypeDefs []
  skipBlk
  symbol "}"
  skipBlk
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
      skipBlk
      t <- lookAhead anyToken
      myTrace ("<< typeDef:2 next token " ++ show t ++ " " ++ show name ++ " " ++ show ty)
      case t of
        TokSymbol "," -> do
          symbol ","
          skipBlk
          optional (symbol ";")
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
