module Language.TypeSystem.ParseType (parseType) where

import Control.Applicative
import Data.Char
import Language.TypeSystem.BaseType
import Language.TypeSystem.Syntax -- あなたの Type 定義
import Text.ParserCombinators.ReadP

parseType :: String -> Type
parseType s =
  case readP_to_S (pType <* skipSpaces <* eof) s of
    [(t, "")] -> t
    _ -> error ("parseType failed: " ++ s)

------------------------------------------------------------
-- パーサ本体
------------------------------------------------------------

pType :: ReadP Type
pType = pFunType

-- A -> B -> C
pFunType :: ReadP Type
pFunType = chainr1 pAppType pArrow
  where
    pArrow = skipSpaces *> string "->" *> skipSpaces *> pure TFun

-- アプリケーション: Parser Pattern
pAppType :: ReadP Type
pAppType = chainl1 pAtomicType (skipSpaces *> pure TApp)

-- 原子型
pAtomicType :: ReadP Type
pAtomicType =
  pList
    <++ pTuple
    <++ pParen
    <++ pConOrVar

{-}
pAtomicType :: ReadP Type
pAtomicType =
  pTuple
    <++ pList
    <++ pParen
    <++ pConOrVar
-}

-- (A,B,C)
pTuple :: ReadP Type
pTuple = do
  char '('
  skipSpaces
  t1 <- pType
  skipSpaces
  ( do
      char ','
      skipSpaces
      ts <- pType `sepBy1` (skipSpaces *> char ',' *> skipSpaces)
      skipSpaces
      char ')'
      return (TTuple (t1 : ts))
    )
    <++ ( do
            char ')'
            return t1
        )

{-}
pTuple :: ReadP Type
pTuple = do
  char '('
  skipSpaces
  ts <- pType `sepBy1` (skipSpaces *> char ',' *> skipSpaces)
  skipSpaces
  char ')'
  case ts of
    [t] -> return t
    _ -> return (TTuple ts)
-}

-- [A]
pList :: ReadP Type
pList = do
  char '['
  skipSpaces
  t <- pType
  skipSpaces
  char ']'
  return (TList t)

-- (A)
pParen :: ReadP Type
pParen = do
  char '('
  skipSpaces
  -- 特別ケース: ()
  ( do
      char ')'
      return (TTuple [])
    )
    <++ ( do
            t <- pType
            skipSpaces
            char ')'
            return t
        )

{-}
pParen :: ReadP Type
pParen = do
  char '('
  skipSpaces
  t <- pType
  skipSpaces
  char ')'
  return t
-}

-- Token / Pattern / Parser / Maybe / Expr / Type
pConOrVar :: ReadP Type
pConOrVar = do
  ident <- munch1 (\c -> c `elem` (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "_"))
  return $
    if isUpper (head ident)
      then TCon ident
      else TVar ident

pArrow :: ReadP (Type -> Type -> Type)
pArrow =
  skipSpaces *> string "->" *> skipSpaces *> pure TFun
