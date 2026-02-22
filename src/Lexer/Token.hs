{-# LANGUAGE OverloadedStrings #-}

module Lexer.Token
  ( Token (..),
  )
where

{-}
import Control.Applicative
import Data.Char (isUpper)
import Data.Void
-- import Text.Megaparsec.Char (oneOf)
import Text.Megaparsec (ParseErrorBundle, Parsec, choice, eof, manyTill, oneOf, parse, satisfy, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
-}

-- Parser type
-- type Parser = Parsec Void String

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
  | TokChar Char
  | TokForall
  | TokEllipsis
  | TokArrow
  | TokLParen
  | TokRParen
  | TokNewline
  | TokInt Int
  | TokSpace Int
  | TokIndent
  | TokVLBrace
  | TokVRBrace
  | TokVSemicolon
  | TokVNewline (Int, Int)
  | TokLambdaCase
  | TokUnknown Char
  deriving (Show, Eq, Ord)
