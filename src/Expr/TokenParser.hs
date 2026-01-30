{-# LANGUAGE LambdaCase #-}

module Expr.TokenParser
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
  )
where

import Control.Applicative (empty)
import qualified Data.Set as Set
import Expr.Combinator
import Lexer (Token (..))
import MyTrace (myTrace)

-- import Text.Megaparsec (token, (<?>))

(<?>) :: Parser a -> String -> Parser a
p <?> _ = p

braces :: Parser a -> Parser a
braces p = between (symbol "{") (symbol "}") p

parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

brackets :: Parser a -> Parser a
brackets p = between (symbol "[") (symbol "]") p

{-}
ident :: Parser String
ident = do
  notFollowedBy (symbol "|")
  notFollowedBy (token TokArrow)
  t <- satisfy isIdent
  myTrace ("<< ident token: " ++ show t)
  case t of
    TokIdent s -> pure s
    -- TokKeyword "return" -> myTrace "<< ident: return" >> pure "return"
    _ -> empty
  where
    isIdent (TokIdent _) = True
    -- isIdent (TokKeyword "return") = True
    isIdent _ = False
-}
ident :: Parser String
ident = tokenIs $ \case
  TokIdent s -> Just s
  _ -> Nothing

int :: Parser Int
int = do
  t <- satisfy isNumber
  case t of
    TokNumber n -> myTrace ("<< int: " ++ show n) >> pure n
    -- TokNumber n -> pure n
    _ -> empty
  where
    isNumber (TokNumber _) = True
    isNumber _ = False

keyword :: String -> Parser ()
keyword kw = do
  t <- satisfy isKeyword
  case t of
    TokKeyword s | s == kw -> myTrace ("<< keyword: " ++ s) >> pure ()
    -- TokKeyword s | s == kw -> pure ()
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
