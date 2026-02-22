
module Test(main, max', Expr(..)) where

import AST.Expr (Expr(..), BinOp, (<|>))
import Data.List
import qualified Data.Map
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Char hiding (isDigit)
import Data.Maybe (..)  -- ← これ！

-- data AAA = {A,B}
-- type T = TokString

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
