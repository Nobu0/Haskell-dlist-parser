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
