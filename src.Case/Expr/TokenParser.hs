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
import Debug.Trace (trace)
import Expr.Combinator
import Lexer (Token (..))

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
ident = tokenIs $ \case
  TokIdent s -> Just s
  _ -> Nothing
-}

ident :: Parser String
ident = do
  t <- satisfy isIdent
  case t of
    TokIdent s -> trace (">> ident: " ++ show s) (pure s)
    _ -> empty
  where
    isIdent (TokIdent _) = True
    isIdent _ = False

int :: Parser Int
int = tokenIs $ \case
  TokNumber n -> Just n
  _ -> Nothing

{-}
keyword :: String -> Parser ()
keyword kw = tokenIs $ \case
  TokKeyword s | s == kw -> Just ()
  _ -> Nothing
-}

keyword :: String -> Parser ()
keyword kw = do
  t <- satisfy isKeyword
  case t of
    TokKeyword s | s == kw -> trace (">> keyword: " ++ s) (pure ())
    _ -> empty
  where
    isKeyword (TokKeyword _) = True
    isKeyword _ = False

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
