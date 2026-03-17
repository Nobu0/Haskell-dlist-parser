{-# LANGUAGE OverloadedStrings #-}

module Lexer.Token
  ( Token (..),
  )
where

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
  | TokVNl
  | TokLambdaCase
  | TokUnknown Char
  deriving (Show, Eq, Ord)

data SourcePos = SourcePos
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq, Ord)

data LocatedToken = LocatedToken
  { tokenPos :: SourcePos,
    token :: Token
  }
  deriving (Show, Eq, Ord)
