module Main where

import AST.Expr

data AAA = {A,B}
type T = TokString

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myprint :: String -> String
myprint str = str ++ "abc"

main = do
  print (myprint "THIS is a sample Haskell file.")
  print ("THIS is a sample Haskell file." ++ "test add")
