{-# LANGUAGE LambdaCase #-}

module Parser.Core.TokenParser
  ( ident,
    int,
    keyword,
    symbol,
    tokenIs,
    parens,
    brackets,
    braces,
    notFollowedBy,
    lookAhead,
    (<?>),
    anyToken,
    stringLiteralExpr,
    skipNewlines,
    skipSeparators,
  )
where

import Control.Applicative (empty, many)
import qualified Data.Set as Set
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Utils.MyTrace (myTrace)

-- import Text.Megaparsec (token, (<?>))

(<?>) :: Parser a -> String -> Parser a
p <?> _ = p

braces :: Parser a -> Parser a
braces p = between (symbol "{") (symbol "}") p

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

brackets :: Parser a -> Parser a
brackets p = between (symbol "[") (symbol "]") p

ident :: Parser String
ident = tokenIs $ \case
  TokIdent s -> Just s
  _ -> Nothing

int :: Parser Int
int = do
  t <- satisfy isNumber
  case t of
    -- TokNumber n -> myTrace ("<< int: " ++ show n) >> pure n
    TokNumber n -> pure n
    -- TokNumber n -> pure n
    _ -> empty
  where
    isNumber (TokNumber _) = True
    isNumber _ = False

keyword :: String -> Parser ()
keyword kw = do
  t <- satisfy isKeyword
  case t of
    -- TokKeyword s | s == kw -> myTrace ("<< keyword: " ++ s) >> pure ()
    TokKeyword s | s == kw -> pure ()
    _ -> empty
  where
    isKeyword (TokKeyword _) = True
    isKeyword _ = False

symbol :: String -> Parser ()
symbol s = tokenIs $ \case
  TokSymbol s' | s' == s -> Just ()
  TokOperator s' | s' == s -> Just ()
  _ -> Nothing

tokenIs :: (Token -> Maybe a) -> Parser a
tokenIs f = Parser $ \case
  (t : ts) -> case f t of
    Just a -> Just (a, ts)
    Nothing -> Nothing
  [] -> Nothing

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \input ->
  case runParser p input of
    Nothing -> Just ((), input) -- p が失敗 → 成功
    Just _ -> Nothing -- p が成功 → 失敗

lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \input ->
  case p input of
    Just (a, _) -> Just (a, input) -- 結果はそのまま、入力は消費しない
    Nothing -> Nothing

anyToken :: Parser Token
anyToken = Parser $ \input ->
  case input of
    (t : ts) -> Just (t, ts)
    [] -> Nothing

debugPeek :: Parser ()
debugPeek = do
  t <- peekToken
  Parser $ \tokens ->
    Just ((), tokens)

peekToken :: Parser Token
peekToken = Parser $ \tokens -> case tokens of
  [] -> Nothing
  (t : _) -> Just (t, tokens)

stringLiteralExpr :: Parser String
stringLiteralExpr =
  satisfyToken f
  where
    f (TokString s) = Just s
    f _ = Nothing

satisfyToken :: (Token -> Maybe a) -> Parser a
satisfyToken f = Parser $ \ts -> case ts of
  [] -> Nothing
  (t : ts') -> case f t of
    Just x -> Just (x, ts')
    Nothing -> Nothing

skipSeparators :: Parser ()
skipSeparators = do
  _ <- many (tokenIs isSep)
  return ()
  where
    isSep TokNewline = Just ()
    isSep (TokSymbol ";") = Just ()
    isSep _ = Nothing

skipNewlines :: Parser ()
skipNewlines = do
  _ <- many (tokenIs (\t -> if t == TokNewline then Just () else Nothing))
  return ()
