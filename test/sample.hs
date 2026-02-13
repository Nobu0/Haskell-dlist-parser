
{-}
module Main where

import AST.Expr (Expr(..), BinOp)
import Data.List
import qualified Data.Map
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Char hiding (isDigit)
import Data.Maybe (..)  -- ← これ！

-- data AAA = {A,B}
-- type T = TokString
maximum' :: (Ord a) => [a]-> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
| x > maxTail = x
| otherwise = maxTail
where maxTail = maximum' xs
-}


max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

{-}
fxx x = f 1
  where
    f x = x
-}
myprint :: String -> String
myprint str = str ++ "abc"

main = do
  print (myprint "THIS is a sample Haskell file.")
  print ("THIS is a sample Haskell file." ++ "test add")
