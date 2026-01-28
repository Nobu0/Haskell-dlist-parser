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
  t <- satisfy isIdent
  case t of
    -- TokIdent s -> myTrace ("<< ident: " ++ show s) >> pure s
    TokIdent s -> pure (s)
    TokKeyword "return" -> myTrace "<< ident: return" >> pure "return"
    -- TokKeyword "return" -> pure ("return")
    _ -> empty
  where
    isIdent (TokIdent _) = True
    isIdent (TokKeyword "return") = True
    isIdent _ = False
-}

ident :: Parser String
ident = do
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

{-}
  tokenIs $ \case
  TokNumber n -> Just n
  _ -> Nothing
-}

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

-- keyword :: String -> Parser ()
-- keyword kw = void (satisfy (== TokKeyword kw))

symbol :: String -> Parser ()
symbol s = tokenIs $ \case
  TokSymbol s' | s == s' -> Just ()
  _ -> Nothing

tokenIs :: (Token -> Maybe a) -> Parser a
tokenIs f = Parser $ \case
  (t : ts) -> case f t of
    Just a -> Just (a, ts)
    Nothing -> Nothing
  [] -> Nothing
