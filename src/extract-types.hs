{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import System.FilePath (dropExtension, splitDirectories, takeBaseName)
import System.Process (readProcess)

guessModuleName :: FilePath -> String
guessModuleName path =
  let parts = reverse . takeWhile (/= "build") . reverse . splitDirectories $ dropExtension path
   in concatMap (\p -> if null p then "" else if null parts then p else p ++ ".") (init parts) ++ last parts

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ifacePath] -> do
      output <- readProcess "ghc" ["--show-iface", ifacePath] ""
      let ls = lines output
          typeSigs = extractTypeSigs ls
          classDecls = extractClassDecls ls
          instances = extractInstances ls
          modName = takeBaseName ifacePath -- ここでファイル名からモジュール名を推測！
      BL.putStrLn $
        encode $
          object
            [ "module" .= modName,
              "types" .= typeSigs,
              "classes" .= classDecls,
              "instances" .= instances
            ]
    _ -> putStrLn "Usage: extract-types path/to/MyModule.hi"

{-}

import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import System.Process (readProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ifacePath] -> do
      output <- readProcess "ghc" ["--show-iface", ifacePath] ""
      let ls = lines output
          typeSigs = extractTypeSigs ls
          classDecls = extractClassDecls ls
          instances = extractInstances ls
      BL.putStrLn $
        encode $
          object
            [ "types" .= typeSigs,
              "classes" .= classDecls,
              "instances" .= instances
            ]
    _ -> putStrLn "Usage: extract-types path/to/MyModule.hi"
-}

-- 型シグネチャの抽出
extractTypeSigs :: [String] -> [String]
extractTypeSigs = map clean . filter isTypeSig
  where
    isTypeSig line = "::" `elem` words line
    clean = unwords . takeWhile (/= "--") . words

-- クラス定義の抽出
extractClassDecls :: [String] -> [String]
extractClassDecls = map clean . filter isClassDecl
  where
    isClassDecl line = "class" `elem` words line
    clean = unwords . takeWhile (/= "--") . words

-- インスタンスの抽出
extractInstances :: [String] -> [String]
extractInstances = map clean . filter isInstance
  where
    isInstance line = "instance" `elem` words line
    clean = unwords . takeWhile (/= "--") . words
