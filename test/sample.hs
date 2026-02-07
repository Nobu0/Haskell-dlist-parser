-- module Main where

myprint :: String -> String
myprint str = str ++ "abc"

main = do
  print (myprint "THIS is a sample Haskell file.")
  print ("THIS is a sample Haskell file." ++ "test add")
