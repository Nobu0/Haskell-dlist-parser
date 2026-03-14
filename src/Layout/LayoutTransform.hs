module Layout.LayoutTransform (layoutTransform) where

import Lexer.Token

layoutTransform :: [Token] -> [Token]
layoutTransform toks = go [] toks

-- go (TokVNewline (x, y) : TokVNewline (x2, y2) : rest) =
--   | x == y && x1 == y2 && x == x1 = TokSymbol ";" : go rest
-- \| x == y = TokVNl : go rest
data LayoutContext
  = InDo
  | InCase
  | InLet
  | InWhere
  deriving (Eq, Show, Ord)

type LayoutStack = [(LayoutContext, Int)] -- [(InDo, indentLevel)]

go :: LayoutStack -> [Token] -> [Token]
go _ [] = []
go [] (TokVNewline (x, y) : rest)
  | x == y && x == 0 = TokNewline : go [] rest -- 文脈が空なら単なる改行として扱う
  | otherwise = TokSymbol ";" : go [] rest
go stack (TokKeyword "do" : TokVNewline (x, y) : rest)
  | y > x = TokKeyword "do" : TokVLBrace : go ((InDo, y) : stack) rest
  | otherwise = TokKeyword "do" : go stack (TokVNewline (x, y) : rest)
go stack (TokKeyword "case" : TokVNewline (x, y) : rest)
  | y > x = TokKeyword "case" : TokVLBrace : go ((InCase, y) : stack) rest
  | otherwise = TokKeyword "case" : go stack (TokVNewline (x, y) : rest)
go stack (TokKeyword "let" : TokVNewline (x, y) : rest)
  | y > x = TokKeyword "let" : TokVLBrace : go ((InLet, y) : stack) rest
  | otherwise = TokKeyword "let" : go stack (TokVNewline (x, y) : rest)
go stack (TokKeyword "where" : TokVNewline (x, y) : rest)
  | y > x = TokKeyword "where" : TokVLBrace : go ((InWhere, y) : stack) rest
  | otherwise = TokKeyword "where" : go stack (TokVNewline (x, y) : rest)
go stack (TokVNewline (x, y) : rest)
  | y == currentIndent stack = TokSymbol ";" : go stack rest
  | y < currentIndent stack =
      let (braces, newStack) = popBraces y stack
       in braces ++ go newStack rest
  | otherwise = TokSymbol ";" : go stack rest -- y > currentIndent → すでに VLBrace 済み
go ctx (t : rest) =
  t : go ctx rest

-- 現在のインデントを取得（スタックが空なら0）
currentIndent :: LayoutStack -> Int
currentIndent [] = 0
currentIndent ((_, i) : _) = i

-- 現在の文脈（スタックトップ）を取得（空ならデフォルト）
headContext :: LayoutStack -> LayoutContext
headContext [] = InDo -- 適当なデフォルト（使われない前提）
headContext ((ctx, _) : _) = ctx

-- 複数のVRBraceを挿入しつつスタックを更新
popBraces :: Int -> LayoutStack -> ([Token], LayoutStack)
popBraces y [] = ([], [])
popBraces y ((ctx, i) : rest)
  | y < i =
      let (braces, newStack) = popBraces y rest
       in (TokVRBrace : TokNewline : braces, newStack)
  | otherwise = ([], (ctx, i) : rest)

{-}
go stack (TokVNewline (x, y) : rest) =
  let (braces, newStack) = popBraces y stack
   in if y > currentIndent stack
        then TokVLBrace : go ((headContext stack, y) : newStack) rest
        else
          if y == currentIndent stack
            then TokSymbol ";" : go newStack rest
            else braces ++ go newStack rest
go :: LayoutStack -> [Token] -> [Token]
go stack (TokKeyword "do" : TokVNewline (_, y) : rest) =
  TokKeyword "do" : TokVNewline (y, y) : go ((InDo, y) : stack) rest
go stack (TokKeyword "case" : TokVNewline (_, y) : rest) =
  TokKeyword "case" : TokVNewline (y, y) : go ((InCase, y) : stack) rest
go stack (TokKeyword "let" : TokVNewline (_, y) : rest) =
  TokKeyword "let" : TokVNewline (y, y) : go ((InLet, y) : stack) rest
go stack (TokKeyword "where" : TokVNewline (_, y) : rest) =
  TokKeyword "where" : TokVNewline (y, y) : go ((InWhere, y) : stack) rest
go ((ctxTop, indentTop) : stack) (TokVNewline (x, y) : rest)
  | y > indentTop = TokVLBrace : go ((ctxTop, y) : stack) rest
  | y == indentTop = TokSymbol ";" : go ((ctxTop, indentTop) : stack) rest
  | y < indentTop = TokVRBrace : go (popUntil y stack) rest
go ctx (t : rest) =
  t : go ctx rest

popUntil :: Ord b => b -> [(a, b)] -> [(a, b)]
popUntil y [] = []
popUntil y ((ctx, i) : rest)
  | y < i = popUntil y rest
  | otherwise = (ctx, i) : rest

go :: [LayoutContext] -> [Token] -> [Token]
go _ [] = []
go ctx (TokKeyword "do" : rest) =
  TokKeyword "do" : go (InDo : ctx) rest
go ctx (TokKeyword "case" : rest) =
  TokKeyword "case" : go (InCase : ctx) rest
go ctx (TokKeyword "let" : rest) =
  TokKeyword "let" : go (InLet : ctx) rest
go ctx (TokKeyword "where" : rest) =
  TokKeyword "where" : go (InWhere : ctx) rest
go (InDo : ctx) (TokVNewline (x, y) : rest)
  | x < y = TokVLBrace : go ctx rest
  | x == y = TokSymbol ";" : go (InDo : ctx) rest
  | x > y = TokVRBrace : go ctx rest
go (InCase : ctx) (TokVNewline (x, y) : rest)
  | x < y = TokVLBrace : go ctx rest
  | x == y = TokSymbol ";" : go (InCase : ctx) rest
  | x > y = TokVRBrace : go ctx rest
go (InLet : ctx) (TokVNewline (x, y) : rest)
  | x < y = TokVLBrace : go ctx rest
  | x == y = TokSymbol ";" : go (InLet : ctx) rest
  | x > y = TokVRBrace : go ctx rest
go (InWhere : ctx) (TokVNewline (x, y) : rest)
  | x < y = TokVLBrace : go ctx rest
  | x == y = TokSymbol ";" : go (InWhere : ctx) rest
  | x > y = TokVRBrace : go ctx rest
go ctx (t : rest) =
  t : go ctx rest

closeBlocks :: [LayoutContext] -> (Int, Int) -> [Token] -> [Token]
closeBlocks ctx (x, y) rest
  | x > y = TokVRBrace : closeBlocks ctx (x - 1, y) rest
  | otherwise = TokNewline : go ctx rest

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
-}