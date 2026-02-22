{-# LANGUAGE LambdaCase #-}

module Parser.Type.TypeParser
  ( parseType,
    parseForall,
    arrowType,
    typeApp,
    typeAtom,
    typeIdent,
    constraintP,
    constraintList,
    typeP,
  )
where

import AST.Decl
import AST.Expr
import AST.Type
import Control.Applicative
import Data.Char (isUpper)
import Data.List (isPrefixOf)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Core.TokenParser (skipNewlines)
import Utils.MyTrace

parseType :: Parser Type
parseType = do
  t <- lookAhead anyToken
  myTrace ("<< parseType dispatch: " ++ show t)
  case t of
    TokVLBrace -> bracesV parseTypeCore
    _ -> parseTypeCore

parseTypeCore :: Parser Type
parseTypeCore = do
  skipMany (symbol ";" <|> newline)
  -- t <- lookAhead anyToken
  -- myTrace ("<< parseTypeCore next token: " ++ show t)
  t1 <- arrowType
  myTrace ("<< parseTypeCore: " ++ show t1)
  rest <- optional $ do
    token (TokKeyword "=>")
    t2 <- parseTypeCore
    return (t1, t2)
  case rest of
    Just (TApp (TCon cls) arg, body) ->
      return $ TConstraint [Constraint cls [arg]] body
    Just (TCon cls, body) ->
      return $ TConstraint [Constraint cls []] body
    Just (TTuple cs, body) ->
      return $ TConstraint (map toConstraint cs) body
    Nothing -> return t1
    _ -> empty

arrowType :: Parser Type
arrowType = do
  t1 <- typeApp
  -- t <- lookAhead anyToken
  -- myTrace ("<< arrowType next token: " ++ show t)
  rest <- optional $ do
    token (TokArrow)
    parseTypeCore
  return $ maybe t1 (TFun t1) rest

parensTuple :: Parser Type
parensTuple = do
  symbol "("
  ts <- parseTypeCore `sepBy1` symbol ","
  symbol ")"
  return (TTuple ts)

toConstraint :: Type -> Constraint
toConstraint (TApp (TCon cls) arg) = (Constraint cls [arg])
toConstraint (TCon cls) = (Constraint cls [])
toConstraint other = error $ "Invalid constraint form: " ++ show other

typeApp :: Parser Type
typeApp = do
  f <- typeTerm -- typeAtom
  args <- many typeTerm -- typeAtom
  return $ foldl TApp f args

typeTerm :: Parser Type
typeTerm = do
  ts <- some typeAtom
  return (foldl1 TApp ts)

typeAtom :: Parser Type
typeAtom =
  (parens parseTypeCore) -- 括弧付き型
    <|> tUnitType -- (symbol "()" *> pure TUnit) -- 単位型
    <|> (TCon <$> typeIdent)
    <|> (TVar <$> ident)
    <|> brackets (TList <$> parseTypeCore)
    <|> parensTuple

tUnitType :: Parser Type
tUnitType = do
  symbol "("
  symbol ")"
  return (TUnit)

constrainedType :: Parser Type
constrainedType = do
  symbol "("
  cs <- sepBy1 constraint (symbol ",")
  symbol ")"
  keyword "=>"
  ty <- arrowType
  return (TConstraint cs ty)

parseForall :: Parser Type
parseForall = do
  token TokForall
  vars <- some ident
  token (TokOperator ".")
  body <- parseTypeCore
  return $ TForall vars body

constraint :: Parser Constraint
constraint = do
  cls <- ident
  ty <- typeApp
  return (Constraint cls [ty])

constraintP :: Parser Constraint
constraintP = do
  className <- typeIdent
  args <- some typeAtom
  return $ Constraint className args

constraintList :: Parser [Constraint]
constraintList = do
  try (parens (constraintP `sepBy1` symbol ","))
    <|> fmap (: []) constraintP

typeP :: Parser Type
typeP = try forallType <|> typeAtom

forallType :: Parser Type
forallType = do
  token TokForall
  vars <- some ident
  token (TokOperator ".")
  t <- constrainedType
  return (TForall vars t)
