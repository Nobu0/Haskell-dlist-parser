module Language.Tool.TypeParser
  ( parseTypeExpr,
    parseArrowType,
    parseAtomicType,
  )
where

import Language.TypeSystem.BaseType
import Language.TypeSystem.Syntax
import Text.Parsec
import Text.Parsec.String (Parser)

parseTypeExpr :: Parsec String () Type
parseTypeExpr = spaces *> parseArrowType <* spaces

parseArrowType :: Parsec String () Type
parseArrowType = do
  t1 <- parseAppType
  rest <- optionMaybe (try (spaces >> string "->" >> spaces >> parseArrowType))
  return $ maybe t1 (TArrow t1) rest

parseAppType :: Parsec String () Type
parseAppType = do
  ts <- many1 parseAtomicType
  return $ foldl1 TApp ts

parseAtomicType :: Parsec String () Type
parseAtomicType =
  try parseUnitType
    <|> try parseTupleType
    <|> try parseListType
    <|> try parseStar
    <|> try (TCon <$> many1 (alphaNum <|> oneOf "._#") <* spaces)
    <|> try (TVar <$> many1 lower <* spaces)
    <|> between (char '(' >> spaces) (spaces >> char ')') parseTypeExpr

parseStar :: Parsec String () Type
parseStar = do
  _ <- char '*' >> spaces
  return $ TCon "*"

parseUnitType :: Parsec String () Type
parseUnitType = do
  _ <- string "(" >> spaces
  _ <- char ')' >> spaces
  return $ TCon "()"

parseTupleType :: Parsec String () Type
parseTupleType = do
  _ <- char '(' >> spaces
  types <- parseTypeExpr `sepBy1` (spaces >> char ',' >> spaces)
  _ <- spaces >> char ')'
  let tupleCon = TCon $ "(" ++ replicate (length types - 1) ',' ++ ")"
  return $ foldl TApp tupleCon types

{-}
parseTupleType :: Parsec String () Type
parseTupleType = do
  _ <- char '(' >> spaces
  t1 <- parseTypeExpr
  _ <- spaces >> char ',' >> spaces
  t2 <- parseTypeExpr
  _ <- spaces >> char ')'
  return $ TApp (TApp (TCon "(,)") t1) t2
-}

parseListType :: Parsec String () Type
parseListType = do
  _ <- char '[' >> spaces
  t <- parseTypeExpr
  _ <- spaces >> char ']'
  return $ TApp (TCon "[]") t

parseTypeSig :: ModuleName -> String -> ((ModuleName, Name), Scheme)
parseTypeSig modName line =
  case break (== ':') line of
    (name, _ : ':' : rest) ->
      case parse parseTypeExpr "" rest of
        Left err -> error $ "Type parse error: " ++ show err
        Right ty -> ((modName, trim name), Forall [] [] ty)
    _ -> error "Invalid type signature format"

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
