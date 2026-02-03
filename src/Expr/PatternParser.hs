{-# LANGUAGE LambdaCase #-}

module Expr.PatternParser
  ( pattern,
    patternStart,
    pConstrOrVar,
    pParenOrTuple,
    pList,
    pWildcard,
    pInt,
  )
where

import Control.Applicative
import Data.Char (isUpper)
{-}
pattern :: Parser Pattern
pattern = do
  p <- makeCons
  myTrace ("<< pattern: " ++ show p) (pure p)
-- pattern = makeCons =<< (pAsPattern <|> pAtom)
pattern = (pAsPattern <|> pAtom) >>= makeCons
-}

import Data.Functor (void)
import Expr.AST
import Expr.Combinator
import Expr.TokenParser
import Expr.TokenParser (lookAhead, notFollowedBy)
import Expr.TypeParser (typeIdent)
import Lexer (Token (..))
import MyTrace (myTrace)

pattern :: Parser Pattern
pattern = do
  p <- pAs <|> makeCons
  myTrace ("<< pattern1: (pAsPattern <|> makeCons)" ++ show p)
  -- stopPattern
  -- t <- lookAhead anyToken
  -- myTrace ("<< patten2 next token: stopPattern" ++ show t)
  return p

patternStart :: Parser ()
patternStart =
  void (symbol "_")
    <|> void ident
    <|> void typeIdent
    <|> void int
    <|> void (symbol "(")

stopPattern :: Parser ()
stopPattern =
  lookAhead $
    symbol "|"
      <|> void (token TokArrow)
      <|> void (token TokNewline)
      <|> symbol ";"
      <|> symbol "}"
      <|> keyword "return"
      <|> keyword "case"
      <|> keyword "let"
      <|> keyword "if"
      <|> keyword "do"
      <|> eof

eof :: Parser ()
eof = Parser $ \ts ->
  case ts of
    [] -> Just ((), [])
    _ -> Nothing

makeCons :: Parser Pattern
makeCons = do
  p <- makeApp
  rest p
  where
    rest p =
      ( do
          symbol ":"
          p2 <- makeCons
          return (PCons p p2)
      )
        <|> return p

makeApp :: Parser Pattern
makeApp = do
  p <- pAtom
  ps <- many pAtom
  return (PApp p ps)

{-}
pAtom :: Parser Pattern
pAtom =
  pAs
    <|> pList
    <|> pParenOrTuple
    <|> pConstrOrVar
    <|> pInt
    <|> (symbol "_" >> return PWildcard)
-}

pAtom :: Parser Pattern
pAtom = do
  t <- lookAhead anyToken
  case t of
    TokKeyword _ -> empty -- ★ キーワードはパターンにならない
    _ -> pure ()
  pAs
    <|> pList
    <|> pParenOrTuple
    <|> pConstrOrVar
    <|> pInt
    <|> (symbol "_" >> return PWildcard)

pAs :: Parser Pattern
pAs = do
  name <- ident
  symbol "@"
  pat <- pAtom
  return (PAs name pat)

pConstrOrVar :: Parser Pattern
pConstrOrVar = tokenIs $ \case
  TokIdent name -> Just (PVar name)
  TokTypeIdent name -> Just (PConstr name [])
  _ -> Nothing

isKeyword :: String -> Bool
isKeyword s =
  s
    `elem` [ "case",
             "of",
             "let",
             "in",
             "if",
             "then",
             "else",
             "do",
             "return"
           ]

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

{-}
------------------------------
-- 拡張
-----------------------------
patternAtom :: Parser Pattern
patternAtom =
  pWildcard
    <|> pInt
    <|> pList
    -- <|> pTuple
    <|> pConstrOrVar
    <|> pParenOrTuple

patternApp :: Parser Pattern
patternApp = do
  p <- patternAtom
  ps <- many patternAtom
  return (foldl PApp p ps)

patternCons :: Parser Pattern
patternCons = do
  p1 <- patternApp
  rest p1
  where
    rest p =
      ( do
          token (TokSymbol ":")
          p2 <- patternCons
          return (PCons p p2)
      )
        <|> return p

patternAs :: Parser Pattern
patternAs = do
  TokIdent name <- satisfy isVar
  token (TokSymbol "@")
  pat <- pattern
  return (PAs name pat)
  where
    isVar (TokIdent _) = True
    isVar _ = False

pattern :: Parser Pattern
pattern =
  try patternAs
    <|> patternCons
-}
