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

import Control.Applicative
import Data.Char (isUpper)
import Data.List (isPrefixOf)
import AST.Decl
import AST.Type
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Lexer.Lexer (Token (..))

parseType :: Parser Type
parseType = parseForall <|> arrowType

parseForall :: Parser Type
parseForall = do
  token TokForall
  vars <- some ident
  token TokDot
  body <- parseType
  return $ TForall vars body

arrowType :: Parser Type
arrowType = do
  t1 <- typeApp
  rest <- optional (token TokArrow *> arrowType)
  return $ maybe t1 (TArrow t1) rest

typeApp :: Parser Type
typeApp = do
  f <- typeAtom
  args <- many typeAtom
  return $ foldl TApp f args

typeAtom :: Parser Type
typeAtom =
  (TCon <$> typeIdent)
    <|> (TVar <$> ident)
    <|> brackets (TList <$> typeP)
    <|> parens typeP

typeIdent :: Parser String
typeIdent = satisfyMap $ \case
  TokTypeIdent s -> Just s
  _ -> Nothing

constraintP :: Parser Constraint
constraintP = do
  className <- typeIdent
  args <- some typeAtom
  return $ Constraint className args

constraintList :: Parser [Constraint]
constraintList =
  try (parens (constraintP `sepBy1` symbol ","))
    <|> fmap (: []) constraintP

typeP :: Parser Type
typeP = try forallType <|> constrainedType

forallType :: Parser Type
forallType = do
  token TokForall
  vars <- some ident
  token TokDot
  t <- constrainedType
  return (TForall vars t)

constrainedType :: Parser Type
constrainedType = do
  constraints <- optional $ try $ do
    cs <- constraintList
    symbol "=>"
    return cs
  t <- arrowType
  return $ maybe t (`TConstraint` t) constraints
