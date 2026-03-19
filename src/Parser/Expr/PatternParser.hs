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

pattern :: Parser Pattern
pattern = do
  ct <- getRemainingCount
  myTrace ("<< pattern: ct=" ++ show ct)
  p <- pAs <|> makeCons
  ct <- getRemainingCount
  myTrace ("<< pattern:\n    (pAs <|> makeCons)" ++ show p ++ " ct=" ++ show ct)
  -- stopPattern
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

{-}
makeApp :: Parser Pattern
makeApp = do
  p <- pAtom
  ps <- many pAtom
  return (PApp p ps)
-}
makeApp :: Parser Pattern
makeApp = do
  p <- pAtom
  ps <- many pAtom
  return $ case ps of
    [] -> p
    _ -> PApp p ps

pAtom :: Parser Pattern
pAtom = do
  t <- lookAhead anyToken
  case t of
    TokKeyword _ -> empty -- ★ キーワードはパターンにならない
    _ -> pure ()
  pAs
    <|> pEmptyList
    <|> pList
    <|> pParenOrTuple
    <|> pConstrOrVar
    <|> pInt
    <|> pChar
    <|> pString
    <|> (symbol "_" >> return PWildcard)

pPattern :: Parser Pattern
pPattern = do
  pat <- pInfix <|> pattern
  myTrace ("<< pPattern:\n    prased " ++ show pat)
  return pat

pInfix :: Parser Pattern
pInfix = chainl1 pAtom infixOp
  where
    infixOp = do
      ct <- getRemainingCount
      myTrace ("<< pInfix: ct=" ++ show ct)
      op <- try operatorI <|> operatorIAsName
      myTrace ("<< pInfix: op " ++ show op ++ " ct=" ++ show ct)
      return (\a b -> PInfix a op b)

pAs :: Parser Pattern
pAs = do
  name <- ident
  symbol "@"
  pat <- pAtom
  return (PAs name pat)

patternVar :: Parser Pattern
patternVar = tokenIs $ \case
  TokIdent name -> Just (PVar name)
  -- TokTypeIdent name -> Just (PConstr name [])
  _ -> Nothing

constraintP :: Parser Pattern
constraintP = tokenIs $ \case
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
  -- pats <- tupleCore
  pats <- pattern `sepBy1` symbol ","
  return $ case pats of
    [single] -> single
    _ -> PTuple pats

tupleCore :: Parser Pattern
tupleCore = do
  -- e1 <- pattern
  -- optional $ symbol ","
  es <- pattern `sepBy1` symbol ","
  return (PTuple es) -- (e1 : es))

pList :: Parser Pattern
pList = PList <$> brackets (pattern `sepBy` symbol ",")

pEmptyList :: Parser Pattern
pEmptyList = do
  symbol "["
  symbol "]"
  return (PConstr "[]" [])

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

pConstrOrVar :: Parser Pattern
pConstrOrVar = do
  name <- qualifiedIdent
  if isUpper (head (last (wordsWhen (== '.') name)))
    then return (PConstr name [])
    else return (PVar name)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where
        (w, s'') = break p s'
