{-# LANGUAGE LambdaCase #-}

module Expr.PatternParser
  ( pattern,
    pConstrOrVar,
    pParenOrTuple,
    pList,
    pWildcard,
    pInt,
  )
where

import Control.Applicative
import Data.Char (isUpper)
import Expr.AST
import Expr.Combinator
import Expr.TokenParser
import Lexer (Token (..))
import MyTrace (myTrace)

{-}
pattern :: Parser Pattern
pattern = do
  p <- makeCons
  myTrace ("<< pattern: " ++ show p) (pure p)
-- pattern = makeCons =<< (pAsPattern <|> pAtom)
pattern = (pAsPattern <|> pAtom) >>= makeCons
-}
pattern :: Parser Pattern
pattern = do
  p <- pAsPattern <|> makeCons
  myTrace ("<< pattern: " ++ show p) >> pure p

makeCons :: Parser Pattern
makeCons = do
  hd <- pAtom
  tl <- many pAtom
  return (foldl PApp hd tl)

pAtom :: Parser Pattern
pAtom =
  try
    ( do
        name <- ident
        symbol "@"
        pat <- pattern
        return (PAs name pat)
    )
    <|> pList
    <|> pParenOrTuple
    <|> pConstrOrVar
    <|> pInt
    <|> (symbol "_" >> return PWildcard)

pAsPattern :: Parser Pattern
pAsPattern = do
  name <- ident
  symbol "@"
  pat <- pAtom
  return (PAs name pat)

pConstrOrVar :: Parser Pattern
pConstrOrVar = tokenIs $ \case
  TokIdent name -> Just (PVar name)
  TokTypeIdent name -> Just (PConstr name [])
  -- TokTypeIdent name -> Just (PConstr name)
  _ -> Nothing

isIdentOnly :: Token -> Bool
isIdentOnly (TokIdent _) = True
isIdentOnly _ = False

pParenOrTuple :: Parser Pattern
pParenOrTuple = parens $ do
  pats <- pattern `sepBy1` symbol ","
  return $ case pats of
    [single] -> single
    _ -> PTuple pats

pList :: Parser Pattern
pList = PList <$> brackets (pattern `sepBy` symbol ",")

pWildcard :: Parser Pattern
pWildcard = symbol "_" >> return PWildcard

pInt :: Parser Pattern
pInt = PInt <$> int
