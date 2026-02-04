{-# LANGUAGE OverloadedStrings #-}

module Lexer.Lexer
  ( Token(..)
  , runLexer
  ) where

import Control.Applicative
import Data.Char (isUpper)
import Data.Void
import Text.Megaparsec (Parsec, parse, manyTill, try, (<|>), ParseErrorBundle, choice, eof)
-- import Text.Megaparsec.Char (oneOf)
import Text.Megaparsec (oneOf, satisfy)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)

import qualified Text.Megaparsec.Char.Lexer as L

-- Parser type
type Parser = Parsec Void String

-- Token definition
data Token
  = TokKeyword String
  | TokIdent String
  | TokTypeIdent String
  | TokNumber Int
  | TokFloat Double
  | TokString String
  | TokSymbol String
  | TokOperator String
  | TokForall
  | TokDot
  | TokEllipsis
  | TokArrow
  | TokLParen
  | TokRParen
  | TokNewline
  deriving (Show, Eq, Ord)

------------------------------------------------------------
-- Space consumer (コメント・空白をスキップ)
------------------------------------------------------------

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")

-- lexeme: トークンの後に sc を実行
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- symbol: 記号を読み、後ろに sc を実行
symbol :: String -> Parser String
symbol = L.symbol sc

------------------------------------------------------------
-- Token parsers
------------------------------------------------------------

-- 文字列リテラル
stringLiteral :: Parser Token
stringLiteral =
  TokString <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- 数値
number :: Parser Token
number =
  lexeme $
    try (TokFloat <$> L.float)
    <|> TokNumber <$> L.decimal

-- 識別子
identifier :: Parser Token
identifier = lexeme $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  let name = first : rest
  return $ case name of
    "forall" -> TokForall
    "do"     -> TokKeyword "do"
    "let"    -> TokKeyword "let"
    "in"     -> TokKeyword "in"
    "if"     -> TokKeyword "if"
    "then"   -> TokKeyword "then"
    "else"   -> TokKeyword "else"
    "case"   -> TokKeyword "case"
    "of"     -> TokKeyword "of"
    "data"   -> TokKeyword "data"
    "where"  -> TokKeyword "where"
    "module" -> TokKeyword "module"
    _ | isUpper first -> TokTypeIdent name
      | otherwise     -> TokIdent name

-- 記号
symbolToken :: Parser Token
symbolToken =
  choice
    [ TokArrow <$ symbol "->"
    , TokEllipsis <$ symbol "..."
    , TokDot <$ symbol "."
    , TokSymbol "::" <$ symbol "::"
    , TokSymbol "<-" <$ symbol "<-"
    , TokSymbol "==" <$ symbol "=="
    , TokSymbol "/=" <$ symbol "/="
    , TokSymbol "<=" <$ symbol "<="
    , TokSymbol ">=" <$ symbol ">="
    , TokSymbol <$> lexeme (string "=" <|> string "(" <|> string ")" <|> string "{" <|> string "}")
    ]

-- 演算子
operator :: Parser Token
operator =
  lexeme $
    choice
      [ TokOperator <$> string "=="
      , TokOperator <$> string "<="
      , TokOperator <$> string ">="
      , TokOperator <$> string "/="
      , TokOperator <$> some (satisfy isOp)
      ]
  where
    isOp c = c `elem` ("+-*/=<>&|" :: String)

{-}
operator :: Parser Token
operator =
  TokOperator <$> lexeme (some (satisfy (`elem` "+-*/=<>&|")))
operator :: Parser Token
operator =
  TokOperator <$> lexeme (some (oneOf "+-*/=<>&|"))
-}

-- 改行
newlineToken :: Parser Token
newlineToken = TokNewline <$ lexeme (char '\n')

------------------------------------------------------------
-- トークン列パーサー
------------------------------------------------------------

tokenParser :: Parser [Token]
tokenParser =
  many $
    choice
      [ newlineToken
      , stringLiteral
      , number
      , identifier
      , symbolToken
      , operator
      ]

------------------------------------------------------------
-- 実行関数
------------------------------------------------------------

runLexer :: String -> Either (ParseErrorBundle String Void) [Token]
runLexer = parse (sc *> tokenParser <* eof) "<input>"

{-}

{-# LANGUAGE OverloadedStrings #-}

module Lexer.Lexer
  ( Token (..),
    runLexer,
  )
where

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
  | -- | TokDot2
    TokEllipsis
  | TokArrow
  | TokLParen
  | TokRParen
  | TokNewline
  deriving (Show, Eq, Ord)

multiCharSymbols :: [String]
multiCharSymbols =
  ["->", "<-", "::", "==", "/=", ">=", "<=", "++"]

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
  TokString <$> (char '"' *> manyTill L.charLiteral (char '"'))


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
    "for" -> TokKeyword "for"
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
        try (string "...") >> return TokEllipsis,
        try (string "..") >> return (TokSymbol ".."),
        try (string ".") >> return TokDot,
        try (string "::") >> return (TokSymbol "::"),
        try (string "==") >> return (TokSymbol "=="),
        try (string "/=") >> return (TokSymbol "/="),
        try (string "<=") >> return (TokSymbol "<="),
        try (string ">=") >> return (TokSymbol ">="),
        oneOf ("=(){}[]:;,\\'_|@" :: String) >>= \c -> return (TokSymbol [c])
      ]

dotTokens :: Parser Token
dotTokens =
  TokEllipsis <$ string "..."
    <|> TokDot <$ string "."

ellipsis :: Parser Token
ellipsis = TokEllipsis <$ string "..."

dot :: Parser Token
dot = TokDot <$ string "."

newlineToken :: Parser Token
newlineToken = TokNewline <$ char '\n'

tokenParser :: Parser [Token]
tokenParser =
  many $
    lexeme $
      choice
        [ stringLiteral
        , number
        , identifier
        , symbolToken
        , dotTokens
        , operator
        , newlineToken
        ]

runLexer :: String -> Either (ParseErrorBundle String Void) [Token]
runLexer = parse tokenParser "<input>"
-}