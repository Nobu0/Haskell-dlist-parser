{-# LANGUAGE LambdaCase #-}

module Parser.Expr.PatternParser
  ( pattern,
    -- patternParser,
    patternStart,
    pConstrOrVar,
    patternVar,
    constraintP,
    pParenOrTuple,
    pList,
    pWildcard,
    pInt,
    pPattern,
  )
where

import AST.Pattern
import Control.Applicative
import Data.Char (isUpper)
import Data.Functor (void)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Type.TypeParser (typeIdent)
import Utils.MyTrace (myTrace)

{-}
patternParser :: Parser Pattern
patternParser = do
  p <- pAs <|> makeCons
  myTrace ("<< patternParser: (pAs <|> makeCons)" ++ show p)
  -- stopPattern
  -- t <- lookAhead anyToken
  -- myTrace ("<< patten2 next token: stopPattern" ++ show t)
  return p
-}
-- pattern :: Parser Pattern
-- pattern = pPattern

pattern :: Parser Pattern
pattern = do
  p <- pAs <|> makeCons
  myTrace ("<< pattern: (pAs <|> makeCons)" ++ show p)
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
      <|> keyword "in"
      <|> keyword "for"
      <|> keyword "return"
      <|> keyword "case"
      <|> keyword "let"
      <|> keyword "if"
      <|> keyword "do"
      <|> eof

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
    <|> pChar
    <|> pString
    <|> (symbol "_" >> return PWildcard)

pPattern :: Parser Pattern
pPattern = do
  t <- lookAhead anyToken
  myTrace ("<< pPattern: next token " ++ show t)
  pat <- pInfix
  myTrace ("<< pPattern: prased " ++ show pat)
  return pat

pInfix :: Parser Pattern
pInfix = chainl1 pAtom infixOp
  where
    infixOp = do
      op <- operatorIAsName
      return (\a b -> PInfix a op b)

pAs :: Parser Pattern
pAs = do
  name <- ident
  symbol "@"
  pat <- pAtom
  return (PAs name pat)

{-}
pConstrOrVar :: Parser Pattern
pConstrOrVar =
  do
    patternVar
    <|> constraintP
-}
{-}
pConstrOrVar :: Parser Pattern
pConstrOrVar = do
  name <- try identI <|> parens operatorIAsName
  return $
    if isUpper (head name) || isSymbolName name
      then PConstr name []
      else PVar name
-}
pConstrOrVar :: Parser Pattern
pConstrOrVar = do
  t <- lookAhead anyToken
  myTrace ("<< pConstrOrVar: next token " ++ show t)
  name <- try identI <|> try (parens operatorIAsName) <|> operatorI
  return $
    if isSymbolName name
      then PConstr name []
      else PVar name

patternVar :: Parser Pattern
patternVar = tokenIs $ \case
  TokIdent name -> Just (PVar name)
  -- TokTypeIdent name -> Just (PConstr name [])
  _ -> Nothing

constraintP :: Parser Pattern
constraintP = tokenIs $ \case
  -- TokIdent name -> Just (PVar name)
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

pChar :: Parser Pattern
pChar = do
  c <- charLiteralExpr -- すでに定義済みならそれを使う
  return (PChar c)

pString :: Parser Pattern
pString = PString <$> stringLiteralExpr
