{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M

type TypeEnv = M.Map T.Text T.Text

loadTypes :: FilePath -> IO TypeEnv
loadTypes path = do
  content <- TIO.readFile path
  let ls = drop 1 $ T.lines content  -- ヘッダーをスキップ
      pairs = [ let (n, t) = T.breakOn "\t" l in (n, T.drop 1 t) | l <- ls, not (T.null l), T.isInfixOf "\t" l ]
  return $ M.fromList pairs

-- 使用例
main :: IO ()
main = do
  env <- loadTypes "types_cleaned.tsv"
  case M.lookup "extractSQLVars" env of
    Just typ -> putStrLn $ "Type of extractSQLVars: " ++ T.unpack typ
    Nothing  -> putStrLn "Not found."
