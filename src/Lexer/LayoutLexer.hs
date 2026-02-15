module Lexer.LayoutLexer (layoutLexer) where

import Lexer.Token

layoutLexer :: [Token] -> [Token]
layoutLexer toks = go [0] 0 toks -- 追加: parenDepth = 0

go :: [Int] -> Int -> [Token] -> [Token]
go stack _ [] = []
-- コメント行（空白＋コメント＋改行）を空行とみなす
go stack 0 (TokNewline : TokSpace n : TokNewline : rest)
  | n == head stack =
      TokVNewline (level, level) : TokVNewline (level, level) : go stack 0 rest
  where
    level = length stack - 1
-- 空行（TokNewline のあとに TokNewline または EOF）
go stack 0 (TokNewline : TokNewline : rest) =
  go stack 0 (TokNewline : rest)
-- 行頭のスペース（括弧外のみ処理）
go stack 0 (TokNewline : TokSpace n : rest)
  | n == head stack =
      let level = length stack - 1
       in TokVNewline (level, level) : go stack 0 rest
  | n > head stack =
      let stack' = n : stack
          prevLevel = length stack - 1
          newLevel = length stack' - 1
       in TokVNewline (prevLevel, newLevel) : go stack' 0 rest
  | n < head stack =
      let stack' = dropWhile (> n) stack
          prevLevel = length stack - 1
          newLevel = length stack' - 1
       in TokVNewline (prevLevel, newLevel) : go stack' 0 rest
-- 行頭の改行（スペースなし）※括弧外のみ処理
go stack 0 (TokNewline : rest) =
  let stack' = [0]
      prevLevel = length stack - 1
   in TokVNewline (prevLevel, 0) : go stack' 0 rest
-- 括弧内の TokSpace や TokNewline はスキップ
go stack depth (TokSpace _ : rest) =
  go stack depth rest
-- go stack depth (TokSpace _ : rest)
--  | depth > 0 = go stack depth rest
go stack depth (TokNewline : rest)
  | depth > 0 = go stack depth rest
-- 括弧のネスト制御
go stack depth (TokSymbol "(" : rest) =
  TokSymbol "(" : go stack (depth + 1) rest
go stack depth (TokSymbol ")" : rest) =
  TokSymbol ")" : go stack (max 0 (depth - 1)) rest
-- その他のトークン
go stack depth (t : rest) =
  t : go stack depth rest

{-}
module Lexer.LayoutLexer (layoutLexer) where

import Lexer.Token

layoutLexer :: [Token] -> [Token]
layoutLexer toks = go [0] toks

go :: [Int] -> [Token] -> [Token]
go stack [] = []
-- 行頭のスペース
go stack (TokNewline : TokSpace n : rest)
  | n == head stack =
      let level = length stack - 1
       in TokVNewline (level, level) : go stack rest
  | n > head stack =
      let stack' = n : stack
          prevLevel = length stack - 1
          newLevel = length stack' - 1
       in TokVNewline (prevLevel, newLevel) : go stack' rest
  | n < head stack =
      let stack' = dropWhile (> n) stack
          prevLevel = length stack - 1
          newLevel = length stack' - 1
       in TokVNewline (prevLevel, newLevel) : go stack' rest
-- 行頭の改行（スペースなし）
go stack (TokNewline : rest) =
  let stack' = [0]
      prevLevel = length stack - 1
   in TokVNewline (prevLevel, 0) : go stack' rest
-- 行頭以外のスペース（SimpleLexer が出さないなら無視でOK）
go stack (TokSpace _ : rest) =
  go stack rest
-- その他のトークン
go stack (t : rest) =
  t : go stack rest
-}