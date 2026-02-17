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
      -- symbol "{"
      -- fields <- braces (sepBy1Skip fieldDef (symbol ";"))
      fields <- fieldDefs
      -- symbol "}"
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

skipBlk :: Parser ()
skipBlk = do
  optional (token TokVLBrace)
  optional (token TokVRBrace)
  optional (token TokNewline)
  optional (token $ TokSymbol ";")
  return ()

derivingClause :: Parser [Name]
derivingClause = do
  keyword "deriving"
  parens (sepBy1 typeIdent (symbol ",")) <|> fmap pure typeIdent

sepBy1Skip :: Parser a -> Parser sep -> Parser [a]
sepBy1Skip p sep = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< sepBy1Skip: next token " ++ show t)
  skipControlTokens
  x <- p
  xs <- many $ do
    skipControlTokens
    _ <- sep
    skipControlTokens
    p
  return (x : xs)

isControlToken :: Token -> Bool
isControlToken t = case t of
  TokVLBrace -> True
  TokVRBrace -> True
  TokNewline -> True
  TokSymbol ";" -> True
  _ -> False

skipControlTokens :: Parser ()
skipControlTokens = skipMany $ satisfyToken (\t -> if isControlToken t then Just () else Nothing)

{-}
data Constraint
  = Constraint String [Type] -- 通常のコンストラクタ
  | ConstraintRecord String [Field] -- レコード構文のコンストラクタ
  deriving (Show, Eq)

data Field = Field String Type
  deriving (Show, Eq)

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
    derivs <- option [] derivingClause
    myTrace ("<< dataDecl:4 next token " ++ show derivs)
    return $ DeclData typeName typeVars constr derivs

constrs :: Parser Constraint
constrs = do
  try constrS <|> try constrOne <|> constrBlk

constrS :: Parser Constraint
constrS = do
  t <- lookAhead anyToken
  myTrace ("<< constrS: next token " ++ show t)
  firstConstr <- dataConstr
  t1 <- lookAhead anyToken
  myTrace ("<< constrS:1 next token " ++ show t1 ++ " " ++ show firstConstr)
  bracesV3 $ do
    t2 <- lookAhead anyToken
    myTrace ("<< constrS:2 next token " ++ show t2)
    case t2 of
      TokVLBrace -> do empty
      TokSymbol "{" -> do empty
      _ -> do
        restConstrs <- many (try dataConstrRc <|> dataConstrNx)
        skipNewlines -- <|> empty)
        t3 <- lookAhead anyToken
        myTrace ("<< constrS:3 next token " ++ show t3)
        return (firstConstr : restConstrs)

constrBlk :: Parser Constraint
constrBlk = do
  t <- lookAhead anyToken
  myTrace ("<< constrBlk: next token " ++ show t)
  bk <- dataConstrBk
  t2 <- lookAhead anyToken
  myTrace ("<< constrBlk:2 next token " ++ show t2)
  return bk

constrOne :: Parser Constraint
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
  t <- lookAhead anyToken
  case t of
    TokSymbol "{" -> do
      args <- dataConstrBk
      return $ ConstraintRecord name args
    _ -> empty

dataConstrRc2 :: String -> Parser Constraint
dataConstrRc2 name = do
  t <- lookAhead anyToken
  myTrace ("<< dataConstrRc2: next token " ++ show t)
  args <- typeDefBlk
  t2 <- lookAhead anyToken
  myTrace ("<< dataConstrRc2:2 next token " ++ show t2)
  return $ ConstraintRecord name args

dataConstrBk :: String -> Parser Constraint
dataConstrBk name = do
  t <- lookAhead anyToken
  myTrace ("<< dataConstrBk: next token " ++ show t)
  args <- typeDefBlk
  return $ ConstraintRecord name args

typeDefBlk :: Parser Field
typeDefBlk = do
  t <- lookAhead anyToken
  myTrace ("<< typeDef: next token " ++ show t)
  getTypeDefs

getTypeDef :: Parser Field
getTypeDef = do
  skipBlk
  symbol "{"
  skipBlk
  rt <- getTypeDefs ([])
  skipBlk
  symbol "}"
  skipSeparators
  skipBlk
  return rt

getTypeDefs :: Field -> Parser Field
getTypeDefs rs = do
  name <- identI
  symbol "::"
  ty <- typeExpr
  skipBlk
  t <- lookAhead anyToken
  myTrace ("<< typeDef: next token " ++ show t)
  case t of
    TokSymbol "," -> do
      optional (symbol ";")
      getTypeDefs
      return $ Field name (ty : rs)
    TokSymbol "}" -> do
      return $ Field name (ty : rs)
    _ -> empty

-}

typeExpr :: Parser Type
typeExpr = do
  ts <- some typeAtom
  return $ foldl1 TApp ts
