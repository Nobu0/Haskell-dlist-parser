{-# LANGUAGE InstanceSigs #-}

module Parser.Core.Combinator
  ( Parser (..),
    runParser,
    satisfy,
    satisfyMap,
    token,
    tokens,
    between,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    try,
    chainl1,
    many1,
    manyTill,
    choice,
    choice1,
    lookAhead,
  )
where

import Control.Applicative (Alternative (..), many)
import Data.List (isPrefixOf)
import Lexer.Token (Token (..))

-- import app.MyTrace (myTrace)

-- 差分リスト風パーサー
newtype Parser a = Parser {runParser :: [Token] -> Maybe (a, [Token])}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Just (a, rest) -> Just (f a, rest)
      Nothing -> Nothing

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Just (a, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser $ \input ->
    case runParser pf input of
      Just (f, rest1) ->
        case runParser pa rest1 of
          Just (a, rest2) -> Just (f a, rest2)
          Nothing -> Nothing
      Nothing -> Nothing

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \input ->
    case runParser p input of
      Just (a, rest) -> runParser (f a) rest
      Nothing -> Nothing

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ->
    runParser p1 input <|> runParser p2 input

-- 補助関数
satisfy :: (Token -> Bool) -> Parser Token
satisfy f = Parser $ \tokens -> case tokens of
  (t : ts) | f t -> Just (t, ts)
  _ -> Nothing

satisfyMap :: (Token -> Maybe a) -> Parser a
satisfyMap f = Parser $ \tokens -> case tokens of
  (t : ts) -> case f t of
    Just x -> Just (x, ts)
    Nothing -> Nothing
  [] -> Nothing

token :: Token -> Parser Token
token t = satisfy (== t)

tokens :: [Token] -> Parser [Token]
tokens expected = Parser $ \input ->
  if expected `isPrefixOf` input
    then Just (expected, drop (length expected) input)
    else Nothing

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
  ( do
      x <- p
      xs <- many (sep *> p)
      return (x : xs)
  )
    <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  first <- p
  rest <- (sep *> sepBy1 p sep) <|> pure []
  return (first : rest)

sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do
  x <- p
  ( do
      _ <- sep
      xs <- sepEndBy p sep
      return (x : xs)
    )
    <|> pure [x]

sepEndBy :: Parser a -> Parser sep -> Parser [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

try :: Parser a -> Parser a
try p = Parser $ \tokens -> runParser p tokens

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> return x

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go =
      end
        *> pure []
          <|> (:)
        <$> p
        <*> go

choice1 :: Parser a -> Parser a -> Parser a
choice1 p q = Parser $ \input ->
  case runParser p input of
    Just r -> Just r
    Nothing -> runParser q input

choice :: [Parser a] -> Parser a
choice [] = Parser $ \_ -> Nothing
choice (p : ps) = choice1 p (choice ps)

{-}
lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \input ->
  case runParser p input of
    Just (a, _) -> Just (a, input) -- 成功しても input を消費しない
    Nothing -> Nothing
-}

lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \input ->
  case p input of
    Just (a, _) -> Just (a, input) -- 結果はそのまま、入力は消費しない
    Nothing -> Nothing

-- 依存：symbol は TokenParser 側で定義されるため、ここでは定義しない
