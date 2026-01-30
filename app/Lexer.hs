{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( Token (..),
    runLexer,
  )
where

-- import Control.Monad (void)
-- import Data.Functor

-- import Text.Megaparsec

-- import Text.Megaparsec (ParseErrorBundle, Parsec, between, choice, errorBundlePretty, many, manyTill, oneOf, parse, some, try, (<|>))

import Control.Applicative
import Data.Char (isUpper)
import Data.Functor
import Data.Void
import Text.Megaparsec (ParseErrorBundle, Parsec, choice, manyTill, oneOf, parse, try, (<|>))
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

data Token
  = TokKeyword String
  | TokIdent String -- 通常の識別子（小文字始まり）
  | TokTypeIdent String -- 型名（大文字始まり）
  | TokNumber Int
  | TokFloat Double
  | TokString String
  | TokSymbol String
  | TokOperator String
  | TokForall -- "forall" キーワード
  | TokDot
  | TokArrow
  | TokLParen
  | TokRParen
  | TokNewline
  deriving (Show, Eq, Ord)

multiCharSymbols :: [String]
multiCharSymbols =
  ["->", "<-", "::", "==", "/=", ">=", "<=", "++"]

{-}
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")
-}

sc :: Parser ()
sc =
  L.space
    (void $ some (char ' ' <|> char '\t'))
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")

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
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  -- rest <- many (alphaNumChar <|> char '_')
  let name = first : rest
  return $ case name of
    "forall" -> TokForall
    -- "->" -> TokArrow
    "do" -> TokKeyword "do"
    "return" -> TokKeyword "return"
    "let" -> TokKeyword "let"
    "in" -> TokKeyword "in"
    "if" -> TokKeyword "if"
    "then" -> TokKeyword "then"
    "else" -> TokKeyword "else"
    "case" -> TokKeyword "case"
    "of" -> TokKeyword "of"
    "data" -> TokKeyword "data"
    "where" -> TokKeyword "where"
    _
      | isUpper first -> TokTypeIdent name
      | otherwise -> TokIdent name

operator :: Parser Token
operator = lexeme $ try $ do
  op <- choice (map (try . string) (reverse multiCharSymbols ++ map pure "+-*/=<>"))
  return $ TokOperator op

symbolToken :: Parser Token
symbolToken =
  lexeme $
    choice
      [ try (string "->") >> return TokArrow,
        try (string "=>") >> return (TokSymbol "=>"),
        try (string "<-") >> return (TokSymbol "<-"),
        try (string "...") >> return (TokSymbol "..."),
        try (string "..") >> return (TokSymbol ".."),
        try (string ".") >> return TokDot,
        try (string "::") >> return (TokSymbol "::"),
        try (string "==") >> return (TokSymbol "=="),
        try (string "/=") >> return (TokSymbol "/="),
        try (string "<=") >> return (TokSymbol "<="),
        try (string ">=") >> return (TokSymbol ">="),
        oneOf ("=(){}[]:;,\\'_|@" :: String) >>= \c -> return (TokSymbol [c])
      ]

newlineToken :: Parser Token
newlineToken = TokNewline <$ char '\n'

tokenParser :: Parser [Token]
tokenParser =
  many $
    choice
      [ newlineToken,
        stringLiteral,
        number,
        identifier,
        symbolToken,
        operator -- ← operator は identifier より後ろに！
      ]

runLexer :: String -> Either (ParseErrorBundle String Void) [Token]
runLexer = parse tokenParser "<input>"
