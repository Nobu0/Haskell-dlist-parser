-- stack はインデントのリスト --

module Layout.LayoutTransform (layoutTransform) where

import Lexer.Token

layoutTransform :: [Token] -> [Token]
layoutTransform toks = go toks

go :: [Token] -> [Token]
go [] = []
go (TokVNewline (x, y) : rest)
  | x == 0 && x == y = TokNewline : go rest
  | x == y = TokSymbol ";" : go rest
  | x < y = TokSymbol "{" : go rest
  | x > y = closeBlocks (x, y) rest
go (t : rest) =
  t : go rest

closeBlocks :: (Int, Int) -> [Token] -> [Token]
closeBlocks (x, y) rest
  | x > y = TokSymbol "}" : closeBlocks (x - 1, y) rest
  | otherwise = TokNewline : go rest
