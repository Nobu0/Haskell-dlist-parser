module Lexer.SimpleLexer (slexer) where

import Data.Char
import Data.Char (isLetter)
import Lexer.Token

slexer :: String -> [Token]
slexer = go
  where
    go [] = []
    ------------------------------------------------------------
    -- 改行
    ------------------------------------------------------------
    go ('\n' : rest) =
      TokNewline : go rest
    ------------------------------------------------------------
    -- 行コメント "-- ..."
    ------------------------------------------------------------
    go ('-' : '-' : rest) =
      let rest' = dropWhile (/= '\n') rest
       in go rest'
    ------------------------------------------------------------
    -- ブロックコメント "{- ... -}"
    ------------------------------------------------------------
    go ('{' : '-' : rest) =
      let rest' = skipBlockComment rest
       in go rest'
    ------------------------------------------------------------
    -- 空白（スペース）
    ------------------------------------------------------------
    go (' ' : rest) =
      let (spaces, rest') = span (== ' ') rest
       in TokSpace (1 + length spaces) : go rest'
    ------------------------------------------------------------
    -- 文字リテラル
    ------------------------------------------------------------
    go ('\'' : rest) =
      case rest of
        ('\\' : c : '\'' : rest') ->
          TokChar (escapeChar c) : go rest'
        (c : '\'' : rest') ->
          TokChar c : go rest'
        _ ->
          TokUnknown '\'' : go rest
    ------------------------------------------------------------
    -- 文字列リテラル
    ------------------------------------------------------------
    go ('"' : rest) =
      let (str, rest') = readString rest
       in TokString str : go rest'
    ------------------------------------------------------------
    -- 数字
    ------------------------------------------------------------
    go ('-' : c : rest)
      | isDigit c =
          let (digits, rest') = span isDigit rest
           in TokNumber (read (c : digits)) : go rest'
    go (c : rest)
      | isDigit c =
          let (digits, rest') = span isDigit rest
           in TokNumber (read (c : digits)) : go rest'
    ------------------------------------------------------------
    -- 識別子
    ------------------------------------------------------------
    go (c : rest)
      | isLetter c =
          let (letters, rest') = span isIdentChar rest
              name = c : letters
           in classifyIdent name : go rest'
    ------------------------------------------------------------
    -- 複数文字記号
    ------------------------------------------------------------
    go ('-' : '>' : rest) = TokArrow : go rest
    go ('=' : '>' : rest) = TokKeyword "=>" : go rest
    go (':' : ':' : rest) = TokSymbol "::" : go rest
    go ('+' : '+' : rest) = TokOperator "++" : go rest
    -- go ('+' : '+' : rest) = TokSymbol "++" : go rest
    go ('=' : '=' : rest) = TokSymbol "==" : go rest
    go ('/' : '=' : rest) = TokSymbol "/=" : go rest
    go ('<' : '=' : rest) = TokSymbol "<=" : go rest
    go ('>' : '=' : rest) = TokSymbol ">=" : go rest
    go ('<' : '-' : rest) = TokSymbol "<-" : go rest
    go ('.' : '.' : '.' : rest) = TokEllipsis : go rest
    go ('.' : '.' : rest) = TokSymbol ".." : go rest
    go ('.' : rest) = TokDot : go rest
    go ('+' : rest) = TokOperator "+" : go rest
    go ('-' : rest) = TokOperator "-" : go rest
    go ('*' : rest) = TokOperator "*" : go rest
    go ('/' : rest) = TokOperator "/" : go rest
    -- go ('=' : rest) = TokOperator "=" : go rest
    go ('<' : rest) = TokOperator "<" : go rest
    go ('>' : rest) = TokOperator ">" : go rest
    ------------------------------------------------------------
    -- 単一記号
    ------------------------------------------------------------
    go (c : rest)
      | isSymbolChar c =
          TokSymbol [c] : go rest
    ------------------------------------------------------------
    -- 不明文字
    ------------------------------------------------------------
    go (c : rest) =
      TokUnknown c : go rest

    ------------------------------------------------------------
    -- 補助関数
    ------------------------------------------------------------

    -- isIdentChar x = isAlphaNum x || x == '_' || x == '\''
    isIdentChar c = isLetter c || isDigit c || c == '_' || c == '\''

    -- isSymbolChar x = x `elem` "=(){}[]:;,+-*/<>|&."
    isSymbolChar x = x `elem` "=(){}[]:;,\\'_|@&"

    classifyIdent "sql" = TokKeyword "sql"
    classifyIdent "do" = TokKeyword "do"
    classifyIdent "let" = TokKeyword "let"
    classifyIdent "in" = TokKeyword "in"
    classifyIdent "case" = TokKeyword "case"
    classifyIdent "of" = TokKeyword "of"
    classifyIdent "where" = TokKeyword "where"
    classifyIdent "forall" = TokForall
    classifyIdent "for" = TokKeyword "for"
    classifyIdent "if" = TokKeyword "if"
    classifyIdent "then" = TokKeyword "then"
    classifyIdent "else" = TokKeyword "else"
    classifyIdent "module" = TokKeyword "module"
    classifyIdent "import" = TokKeyword "import"
    classifyIdent "return" = TokKeyword "return"
    classifyIdent "data" = TokKeyword "data"
    classifyIdent "class" = TokKeyword "class"
    classifyIdent "type" = TokKeyword "type"
    classifyIdent "newtype" = TokKeyword "newtype"
    classifyIdent "instance" = TokKeyword "instance"
    classifyIdent "as" = TokKeyword "as"
    classifyIdent "qualified" = TokKeyword "qualified"
    classifyIdent "hiding" = TokKeyword "hiding"
    classifyIdent "=>" = TokKeyword "=>"
    classifyIdent "->" = TokArrow
    -- classifyIdent "..." = TokEllipsis
    classifyIdent "." = TokDot
    classifyIdent "::" = TokSymbol "::"
    classifyIdent "<-" = TokSymbol "<-"
    classifyIdent "==" = TokOperator "=="
    classifyIdent "<=" = TokOperator "<="
    classifyIdent ">=" = TokOperator ">="
    classifyIdent "/=" = TokOperator "/="
    classifyIdent name
      | isUpper (head name) = TokTypeIdent name
      | otherwise = TokIdent name

    escapeChar 'n' = '\n'
    escapeChar 't' = '\t'
    escapeChar '\\' = '\\'
    escapeChar '\'' = '\''
    escapeChar c = c

    ------------------------------------------------------------
    -- ブロックコメントスキップ
    ------------------------------------------------------------
    skipBlockComment :: String -> String
    skipBlockComment [] = []
    skipBlockComment ('-' : '}' : rest) = rest
    skipBlockComment (_ : rest) = skipBlockComment rest

    ------------------------------------------------------------
    -- 文字列リテラル読み取り
    ------------------------------------------------------------
    readString :: String -> (String, String)
    readString [] = ("", [])
    readString ('"' : rest) = ("", rest)
    readString ('\\' : c : rest) =
      let (s, rest') = readString rest
       in (escapeChar c : s, rest')
    readString (c : rest) =
      let (s, rest') = readString rest
       in (c : s, rest')

{-}
module Lexer.SimpleLexer (runLexer) where

import Data.Char
import Lexer.Token (Token (..))

runLexer :: String -> [Token]
runLexer = go
  where
    go [] = []
    go ('\n' : rest) =
      TokNewline : go rest
    go (' ' : rest) =
      let (spaces, rest') = span (== ' ') rest
       in TokSpace (1 + length spaces) : go rest'
    go (c : rest)
      | isDigit c =
          let (digits, rest') = span isDigit rest
           in TokNumber (read (c : digits)) : go rest'
      | isAlpha c =
          let (letters, rest') = span isIdentChar rest
              name = c : letters
           in classifyIdent name : go rest'
      | isSymbolChar c =
          TokSymbol [c] : go rest
      | otherwise =
          TokUnknown c : go rest

    isIdentChar x = isAlphaNum x || x == '_' || x == '\''
    isSymbolChar x = x `elem` "=(){}[]:;,+-*/<>|&"

    classifyIdent "do" = TokKeyword "do"
    classifyIdent "let" = TokKeyword "let"
    classifyIdent "in" = TokKeyword "in"
    classifyIdent "case" = TokKeyword "case"
    classifyIdent "of" = TokKeyword "of"
    classifyIdent "where" = TokKeyword "where"
    classifyIdent "forall" = TokForall
    classifyIdent "if" = TokKeyword "if"
    classifyIdent "then" = TokKeyword "then"
    classifyIdent "else" = TokKeyword "else"
    classifyIdent "data" = TokKeyword "data"
    classifyIdent "where" = TokKeyword "where"
    classifyIdent "module" = TokKeyword "module"
    classifyIdent "->" = TokArrow
    classifyIdent "..." = TokEllipsis
    classifyIdent "." = TokDot
    classifyIdent "::" = TokSymbol "::"
    classifyIdent "<-" = TokSymbol "<-"
    classifyIdent "==" = TokOperator "=="
    classifyIdent "<=" = TokOperator "<="
    classifyIdent ">=" = TokOperator ">="
    classifyIdent "/=" = TokOperator "/="
    classifyIdent name
      | isUpper (head name) = TokTypeIdent name
      | otherwise = TokIdent name
-}
