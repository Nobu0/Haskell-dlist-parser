{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( Token (..),
    runLexer,
  )
where

-- import Control.Monad (void)
-- import Data.Functor
import Data.Void
-- import Text.Megaparsec

-- import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, errorBundlePretty, many, manyTill, oneOf, parse, some, try, (<|>))
import Text.Megaparsec (ParseErrorBundle, Parsec, choice, many, manyTill, oneOf, parse, try, (<|>))
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    space1,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

-- import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

{-}
instance Ord Token where
  compare (TokKeyword a) (TokKeyword b) = compare a b
  compare (TokKeyword _) _ = LT
  compare _ (TokKeyword _) = GT
  compare (TokIdent a) (TokIdent b) = compare a b
  compare (TokIdent _) _ = LT
  compare _ (TokIdent _) = GT
  compare (TokNumber a) (TokNumber b) = compare a b
  compare (TokNumber _) _ = LT
  compare _ (TokNumber _) = GT
  compare (TokFloat a) (TokFloat b) = compare (show a) (show b) -- 安全な比較
  compare (TokFloat _) _ = LT
  compare _ (TokFloat _) = GT
  compare (TokString a) (TokString b) = compare a b
  compare (TokString _) _ = LT
  compare _ (TokString _) = GT
  compare (TokSymbol a) (TokSymbol b) = compare a b
  compare (TokSymbol _) _ = LT
  compare _ (TokSymbol _) = GT
  compare (TokOperator a) (TokOperator b) = compare a b
-}

data Token
  = TokKeyword String
  | TokIdent String
  | TokNumber Int
  | TokFloat Double
  | TokString String
  | TokSymbol String
  | TokOperator String
  deriving (Show, Eq, Ord)

keywords :: [String]
keywords =
  ["let", "in", "if", "then", "else", "case", "of", "data", "where"]

multiCharSymbols :: [String]
multiCharSymbols =
  ["->", "<-", "::", "==", "/=", ">=", "<=", "++"]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral :: Parser Token
stringLiteral =
  TokString <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

number :: Parser Token
number =
  try float
    <|> try negInt
    <|> int
  where
    int = TokNumber <$> lexeme L.decimal
    negInt = TokNumber . negate <$> lexeme (char '-' *> L.decimal)
    float = TokFloat <$> lexeme L.float

identifier :: Parser Token
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  return $
    if name `elem` keywords
      then TokKeyword name
      else TokIdent name

operator :: Parser Token
operator = lexeme $ try $ do
  op <- choice (map (try . string) (reverse multiCharSymbols ++ map pure "+-*/"))
  return $ TokOperator op

symbolToken :: Parser Token
symbolToken =
  lexeme $
    choice
      [ try (string "->") >> return (TokSymbol "->"),
        try (string "::") >> return (TokSymbol "::"),
        try (string "==") >> return (TokSymbol "=="),
        try (string "/=") >> return (TokSymbol "/="),
        try (string "<=") >> return (TokSymbol "<="),
        try (string ">=") >> return (TokSymbol ">="),
        oneOf ("=(){}[]:;,\\'<>_" :: String) >>= \c -> return (TokSymbol [c])
      ]

tokenParser :: Parser [Token]
tokenParser =
  many $
    choice
      [ stringLiteral,
        number,
        identifier,
        symbolToken,
        operator -- ← operator は identifier より後ろに！
      ]

runLexer :: String -> Either (ParseErrorBundle String Void) [Token]
runLexer = parse tokenParser "<input>"
