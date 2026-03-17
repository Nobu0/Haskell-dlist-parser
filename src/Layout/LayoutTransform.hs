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
go [] (TokVNewline (x, y) : TokSymbol "`" : rest)
  | x == y && x == 0 = TokNewline : go [] (TokSymbol "`" : rest)
  | otherwise = go [] (TokSymbol "`" : rest)
go [] (TokVNewline (x, y) : TokSymbol "(" : rest)
  | x == y && x == 0 = TokNewline : go [] (TokSymbol "(" : rest)
  | otherwise = go [] (TokSymbol "(" : rest)
go [] (TokNewline : TokSymbol "(" : rest)
  | otherwise = go [] (TokSymbol "(" : rest)
-- \| otherwise = go [] (TokSymbol "(" : rest)
go [] (TokVNewline (x, y) : TokSymbol "[" : rest)
  | x == y && x == 0 = TokNewline : go [] (TokSymbol "[" : rest)
  | otherwise = TokSymbol ";" : go [] (TokSymbol "[" : rest)
-- \| otherwise = go [] (TokSymbol "[" : rest)
go [] (TokSymbol ";" : TokSymbol "`" : rest)
  | otherwise = go [] (TokSymbol "`" : rest)
go [] (TokSymbol ";" : TokSymbol "(" : rest)
  | otherwise = go [] (TokSymbol "(" : rest)
go [] (TokSymbol ";" : TokSymbol "[" : rest)
  | otherwise = go [] (TokSymbol "[" : rest)
go [] (TokVNewline (x, y) : rest)
  | y == 0 = TokNewline : go [] rest
  | x == y && x == 0 = TokNewline : go [] rest
  | otherwise = TokSymbol ";" : go [] rest
go stack (TokKeyword "do" : TokVNewline (x, y) : rest)
  | y > x = TokKeyword "do" : TokVLBrace : go ((InDo, y) : stack) rest
  | otherwise = TokKeyword "do" : go stack (TokVNewline (x, y) : rest)
go stack (TokKeyword "case" : rest) =
  TokKeyword "case" : go ((InCase, -1) : stack) rest
go ((InCase, -1) : stack) (TokVNewline (x, y) : rest)
  | y > x = TokVLBrace : go ((InCase, y) : stack) rest
  | otherwise = go stack (TokVNewline (x, y) : rest)
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
  -- \| otherwise = ((TokNewline : []), (ctx, i) : rest)
  | otherwise = ([], (ctx, i) : rest)
