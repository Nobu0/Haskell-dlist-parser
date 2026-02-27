module Layout.LayoutTransform (layoutTransform) where

import Lexer.Token

layoutTransform :: [Token] -> [Token]
layoutTransform toks = go toks

-- go (TokVNewline (x, y) : TokVNewline (x2, y2) : rest) =
--   | x == y && x1 == y2 && x == x1 = TokSymbol ";" : go rest
-- \| x == y = TokVNl : go rest

go :: [Token] -> [Token]
go [] = []
go (TokVNewline (x, y) : rest)
  | x == 0 && x == y = TokNewline : go rest
  | x == y = TokSymbol ";" : go rest
  | x < y = TokVLBrace : go rest
  | x > y = closeBlocks (x, y) rest
go (t : rest) =
  t : go rest

closeBlocks :: (Int, Int) -> [Token] -> [Token]
closeBlocks (x, y) rest
  | x > y = TokVRBrace : closeBlocks (x - 1, y) rest
  | otherwise = TokNewline : go rest
