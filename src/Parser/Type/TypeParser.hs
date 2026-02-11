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
import Utils.MyTrace

{-}
parseType :: Parser Type
parseType = try constrainedType <|> parseForall <|> arrowType
-}

parseType :: Parser Type
parseType = do
  t1 <- arrowType
  rest <-
    optional
      ( do
          token (TokKeyword "=>")
          t2 <- parseType
          return (t1, t2)
      )
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
  t <- lookAhead anyToken
  myTrace ("<< arrowType: next token " ++ show t)
  t1 <- typeApp
  rest <- optional (token TokArrow *> arrowType)
  return $ maybe t1 (TFun t1) rest

parensWithConstraints :: Parser Type
parensWithConstraints = do
  symbol "("
  ts <- parseType `sepBy1` symbol ","
  next <- lookAhead anyToken
  myTrace ("<< parensWithConstraints: parsed " ++ show ts ++ ", lookAhead: " ++ show next)
  case next of
    TokKeyword "=>" -> do
      token (TokKeyword "=>")
      ty <- parseType
      symbol ")" -- ← ここで閉じる！
      let constraints = map toConstraint ts
      return $ TConstraint constraints ty
    _ -> do
      symbol ")" -- ← 通常の型やタプルとして閉じる
      return $ case ts of
        [t] -> t
        _ -> TTuple ts

toConstraint :: Type -> Constraint
toConstraint (TApp (TCon cls) arg) = (Constraint cls [arg])
toConstraint (TCon cls) = (Constraint cls [])
toConstraint other = error $ "Invalid constraint form: " ++ show other

typeApp :: Parser Type
typeApp = do
  f <- typeAtom
  args <- many typeAtom
  return $ foldl TApp f args

typeAtom :: Parser Type
typeAtom = do
  (TCon <$> typeIdent)
    <|> (TVar <$> ident)
    <|> brackets (TList <$> parseType)
    <|> parensWithConstraints

{-}
-- <|> parens typeInParens

typeInParens :: Parser Type
typeInParens = do
  myTrace ("<< typeInParens:")
  ts <- parseType `sepBy1` symbol ","
  return $ case ts of
    [t] -> t
    _ -> TTuple ts
-}

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
  token TokDot
  body <- parseType
  return $ TForall vars body

typeIdent :: Parser String
typeIdent = satisfyMap $ \case
  TokTypeIdent s -> Just s
  _ -> Nothing

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
  token TokDot
  t <- constrainedType
  return (TForall vars t)

{-}
parseType :: Parser Type
parseType =
  try constrainedType
    <|> parseForall
    <|> arrowType

parseForall :: Parser Type
parseForall = do
  token TokForall
  vars <- some ident
  token TokDot
  body <- parseType
  return $ TForall vars body

arrowType :: Parser Type
arrowType = do
  t1 <- typeP
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

-- tupleOrParens

-- <|> constrainedType

tupleOrParens :: Parser Type
tupleOrParens = do
  ts <- typeAtom `sepBy1` symbol ","
  return $ case ts of
    [t] -> t
    _ -> TTuple ts

constrainedType :: Parser Type
constrainedType = do
  symbol "("
  cs <- sepBy1 constraint (symbol ",")
  symbol ")"
  symbol "=>"
  ty <- arrowType
  return (TConstraint cs ty)

-}
{-}
constrainedType :: Parser Type
constrainedType = do
  constraints <- optional $ try $ do
    cs <- constraintList
    keyword "=>"
    return cs
  t <- arrowType
  return $ maybe t (`TConstraint` t) constraints
-}
