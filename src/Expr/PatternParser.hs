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

pattern :: Parser Pattern
pattern = do
  p <- makeCons
  myTrace ("<< pattern: " ++ show p) (pure p)

makeCons :: Parser Pattern
makeCons = do
  hd <- pAtom
  tl <- many pAtom
  return (foldl PApp hd tl)

pAtom :: Parser Pattern
pAtom =
  myTrace ">> pWildcard" pWildcard
    <|> myTrace ">> pList" pList
    <|> myTrace ">> pParenOrTuple" pParenOrTuple
    <|> myTrace ">> pConstrOrVar" pConstrOrVar
    <|> myTrace ">> pInt" pInt

pConstrOrVar :: Parser Pattern
pConstrOrVar = tokenIs $ \case
  TokIdent name -> Just (PVar name)
  TokTypeIdent name -> Just (PConstr name [])
  -- TokTypeIdent name -> Just (PConstr name)
  _ -> Nothing

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
