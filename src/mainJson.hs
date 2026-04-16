module Main where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Language.Tool.Load
-- import Language.Tool.Load (buildTypeDB, extractTypeSigs, mergeTypeLines)
import Language.Tool.Type
import qualified Language.Tool.Type as T
import Language.TypeSystem.BaseType
import Language.TypeSystem.ClassEnv
import qualified Language.TypeSystem.ClassEnv as CE
import Language.TypeSystem.Syntax
import Language.TypeSystem.TypeDB
import Language.TypeSystem.TypeDB (TypeDB)

main :: IO ()
main = do
  -- JSONファイルを読み込む
  content <- BL.readFile "inferDB5.json"

  -- JSONをパース
  case eitherDecode content of
    Left err -> putStrLn $ "X JSON parse error: " ++ err
    Right modules -> do
      -- 各モジュールの types フィールドを処理
      let mergedModules = map mergeTypes modules
          typeDB = buildTypeDB mergedModules
      print typeDB -- or save it, or use it further

-- 型シグネチャの断片を結合してから抽出
mergeTypes :: TypeDBJson -> TypeDBJson
mergeTypes mod =
  let rawTypes = types mod
      merged = mergeTypeLines rawTypes
      sigs = extractTypeSigs merged
   in mod {types = sigs}

findMainModule :: TypeDB -> Maybe ModuleName
findMainModule db =
  let matches = [modName | ((modName, name), _) <- Map.toList (typeSigs db), name == "main"]
   in case matches of
        (m : _) -> Just m
        [] -> Nothing

{-}
main :: IO ()
main = do
  db <- loadTypeDBFromJson "inferDB5.json"
  let sigs = Map.toList (typeSigs db)
  if null sigs
    then putStrLn "@  No type signatures found in TypeDB!"
    else mapM_ print sigs
  case findMainModule db of
    Just modName -> putStrLn $ "Main is defined in module: " ++ modName
    Nothing -> putStrLn "No main function found in any module!"
-}
