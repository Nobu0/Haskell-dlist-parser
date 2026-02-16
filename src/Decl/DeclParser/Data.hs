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
import Parser.Expr.PatternParser (pattern, patternParser)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

dataDecl :: Parser Decl
dataDecl = do
  keyword "data"
  typeName <- identI
  typeVars <- many identI
  bracesV3 $ do
    t <- lookAhead anyToken
    myTrace ("<< dataDecl: next token " ++ show t)
    -- bracesV3 $ do
    symbol "="
    t2 <- lookAhead anyToken
    myTrace ("<< dataDecl:2 next token " ++ show t2)
    constr <- constrs
    t3 <- lookAhead anyToken
    myTrace ("<< dataDecl:2 next token " ++ show t3 ++ show constr)
    -- restConstrs <- restBlock

    -- bracesV3 $ do
    -- restConstrs <- many (try dataConstrNx <|> dataConstrRc)
    -- restConstrs <- restBlock
    -- myTrace ("<< dataDecl:3 next token" ++ show restConstrs)
    derivs <- option [] derivingClause
    myTrace ("<< dataDecl:4 next token " ++ show derivs)
    return $ DeclData typeName typeVars constr derivs

constrs = do
  constrS <|> try constrOne

-- constrs :: Parser [Type]
constrS = do
  t <- lookAhead anyToken
  myTrace ("<< constrS: next token " ++ show t)
  firstConstr <- dataConstr
  t <- lookAhead anyToken
  myTrace ("<< constrS:1 next token " ++ show t ++ " " ++ show firstConstr)
  bracesV3 $ do
    t2 <- lookAhead anyToken
    myTrace ("<< constrS:2 next token " ++ show t2)
    case t2 of
      TokVLBrace -> do empty
      _ -> do
        -- bracesV3 $ do
        -- restConstrs <- many (try dataConstrNx <|> dataConstrRc)
        restConstrs <- many (try dataConstrRc <|> dataConstrNx <|> empty)
        return (firstConstr : restConstrs)

constrOne = do
  firstConstr <- dataConstrOne
  skipNewlines
  t <- lookAhead anyToken
  myTrace ("<< constrOne: next token " ++ show t ++ " " ++ show firstConstr)
  return (firstConstr : [])

derivingClause :: Parser [Name]
derivingClause = do
  keyword "deriving"
  parens (sepBy1 typeIdent (symbol ",")) <|> fmap pure typeIdent

dataConstr :: Parser Constraint
dataConstr = do
  t <- lookAhead anyToken
  myTrace ("<< dataConstr: next token " ++ show t)
  name <- identI
  args <- many typeAtom
  -- case args of
  --  [] -> do empty
  --  _ -> do
  myTrace ("<< dataConstr:2 " ++ show args)
  optional (symbol ";")
  t <- lookAhead anyToken
  myTrace ("<< dataConstr:3 next token " ++ show t)
  return $ Constraint name args

dataConstrOne :: Parser Constraint
dataConstrOne = do
  t <- lookAhead anyToken
  myTrace ("<< dataConstrOne: next token " ++ show t)
  name <- identI
  bracesV3 $ do
    args <- sepBy1 typeAtom (symbol ";")
    myTrace ("<< dataConstrOne:2 " ++ show args)
    t <- lookAhead anyToken
    myTrace ("<< dataConstrOne:3 next token " ++ show t)
    skipNewlines
    return $ Constraint name args

dataConstrN1 :: String -> Parser Constraint
dataConstrN1 name = do
  t <- lookAhead anyToken
  myTrace ("<< dataConstrN1: next token " ++ show t)
  -- name <- identI
  braces $ do
    args <- sepBy1 typeAtom (symbol ";")
    myTrace ("<< dataConstrN1:2 next token " ++ show args)
    return $ Constraint name args

dataConstrNx :: Parser Constraint
dataConstrNx = do
  t <- lookAhead anyToken
  myTrace ("<< dataConstrNx: next token " ++ show t)
  symbol "|"
  name <- identI
  args <- many typeAtom
  optional (symbol ";")
  return $ Constraint name args

dataConstrRc :: Parser Constraint
dataConstrRc = do
  t <- lookAhead anyToken
  myTrace ("<< dataConstrRc: next token " ++ show t)
  symbol "|"
  name <- identI
  rt <- dataConstrRc2 name
  -- t2 <- lookAhead anyToken
  -- myTrace ("<< dataConstrRc:2 next token" ++ show t2)
  skipNewlines
  return rt

dataConstrRc2 :: String -> Parser Constraint
dataConstrRc2 name = do
  t2 <- lookAhead anyToken
  myTrace ("<< dataConstrRc:2 next token " ++ show t2)
  bracesv $ do
    symbol "{"
    arg <- typeDef
    args <- bracesv $ sepBy1 typeDef (symbol ";")
    skipNewlines
    symbol "}"
    return $ ConstraintRecord name (arg : args)

-- skipNewlines

typeDef :: Parser Field
typeDef = do
  t <- lookAhead anyToken
  myTrace ("<< typeDef: next token " ++ show t)
  name <- identI
  symbol "::"
  ty <- typeExpr
  optional (symbol ",")
  return $ Field name ty

{-}
typeAtom :: Parser Type
typeAtom =
  TyVar <$> identI
    <|> TyCon <$> typeIdent
    <|> parens typeExpr
-}

typeExpr :: Parser Type
typeExpr = do
  ts <- some typeAtom
  return $ foldl1 TApp ts
