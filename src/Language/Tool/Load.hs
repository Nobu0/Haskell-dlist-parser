{-# LANGUAGE DeriveGeneric #-}

module Language.Tool.Load
  ( loadTypeDBFromJson,
    mergeTypeLines,
    buildTypeDB,
    extractTypeSigs,
  )
where

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
-- (isPrefixOf)

import Data.Char (isSpace)
import Data.List
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Language.Tool.Type
import qualified Language.Tool.Type as T
import Language.Tool.TypeParser (parseTypeExpr)
import Language.TypeSystem.BaseType
import Language.TypeSystem.ClassEnv
import qualified Language.TypeSystem.ClassEnv as CE
import Language.TypeSystem.Syntax
import Language.TypeSystem.TypeDB
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec (Parsec, between, many1, parse, spaces, string, (<|>))
import Text.Regex.TDFA ((=~))

addClassEnv :: String -> [String] -> ClassEnv -> ClassEnv
addClassEnv cls _ env =
  env {CE.classes = Map.insert cls [] (CE.classes env)}

loadTypeDBFromJson path = do
  content <- BLC.readFile path
  let linesOfJson = zip [1 ..] (BLC.lines content)
      -- putStrLn $ "@ Loaded " ++ show (length linesOfJson) ++ " lines from " ++ path
      parsed = [(n, BLC.unpack l, decode l :: Maybe TypeDBJson) | (n, l) <- linesOfJson]

  -- forM_ [m | (_, _, Just m) <- parsed] $ \TypeDBJson {module_ = mname, types = ts} -> do
  -- putStrLn $ "Module: " ++ show mname
  -- mapM_ (\t -> putStrLn $ "  type: " ++ t) ts

  -- forM_ linesOfJson $ \(n, l) -> do
  --  putStrLn $ "Line " ++ show n ++ ": " ++ BLC.unpack l

  let parsed = [(n, BLC.unpack l, decode l :: Maybe TypeDBJson) | (n, l) <- linesOfJson]
      failures = [(n, txt) | (n, txt, Nothing) <- parsed]
  if null failures
    then do
      let modules = [m | (_, _, Just m) <- parsed]
      putStrLn $ "L Successfully parsed " ++ show (length modules) ++ " modules."
      return $ buildTypeDB modules
    else do
      putStrLn "X Failed to parse the following JSON lines:"
      forM_ failures $ \(n, txt) -> do
        putStrLn $ "Line " ++ show n ++ ": " ++ txt
      error "Aborting due to JSON parse errors."

{-}
loadTypeDBFromJson :: FilePath -> IO TypeDB
loadTypeDBFromJson path = do
  content <- BL.readFile path
  let linesOfJson = zip [1 ..] (BLC.lines content)
      parsed = [(n, decodeModule l) | (n, l) <- linesOfJson]
      failures = [(n, l) | (n, Nothing) <- parsed, let l = snd (linesOfJson !! (n - 1))]
  if null failures
    then do
      let modules = [m | (_, Just m) <- parsed]
      return $ buildTypeDB modules
    else do
      putStrLn "X Failed to parse the following JSON lines:"
      forM_ failures $ \(n, l) -> do
        putStrLn $ "Line " ++ show n ++ ": " ++ BLC.unpack l
      error "Aborting due to JSON parse errors."
-}
decodeModule :: BL.ByteString -> Maybe TypeDBJson
decodeModule = decode

buildTypeDB :: [TypeDBJson] -> TypeDB
buildTypeDB modules =
  let sigs =
        Map.fromList . concat $
          [ case splitSig sig of
              Just (name, tyStr) ->
                if null (words tyStr)
                  then unsafePerformIO $ do
                    putStrLn $ "!!  Skipping empty type: " ++ show sig
                    return []
                  else case parse parseTypeExpr "" tyStr of
                    Right ty -> [((modName, name), Forall [] [] ty)]
                    Left err -> unsafePerformIO $ do
                      putStrLn $ "X Failed to parse type: " ++ tyStr
                      putStrLn $ "   In module: " ++ modName ++ ", function: " ++ name
                      putStrLn $ "   Error: " ++ show err
                      return []
              Nothing -> unsafePerformIO $ do
                putStrLn $ "!!  Skipping invalid type signature: " ++ sig
                return []
            | TypeDBJson {module_ = Just modName, types = ts} <- modules,
              sig <- ts
          ]
      clsEnv =
        foldr
          addInst
          ( foldr
              addCls
              initialClassEnv
              (concat [cls | TypeDBJson {T.classes = cls} <- modules])
          )
          ( concat
              [ filter ("instance" `isPrefixOf`) insts
                | TypeDBJson {instances = insts} <- modules
              ]
          )
   in TypeDB {typeSigs = sigs, classEnv = clsEnv}

splitSig :: String -> Maybe (String, String)
splitSig sig =
  case break (== ':') sig of
    (name, ':' : ':' : rest) ->
      let trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
       in Just (trim name, trim rest)
    _ -> Nothing

parseScheme :: String -> Scheme
parseScheme s =
  case parse parseTypeExpr "" s of
    Right ty -> Forall [] [] ty
    Left err -> error $ "Parse error in type: " ++ show err

addCls :: String -> ClassEnv -> ClassEnv
addCls line env =
  case words line of
    ("class" : cls : _) -> addClassEnv cls [] env
    _ -> env

addInst :: String -> ClassEnv -> ClassEnv
addInst line env =
  case words line of
    ("instance" : cls : ty : _) -> addInstance cls ty env
    _ -> env

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- 型シグネチャの形式だけを抽出する
extractTypeSigs :: [String] -> [String]
extractTypeSigs = mapMaybe extract
  where
    extract line =
      case break (== ':') line of
        (name, ':' : ':' : rest) ->
          let trim = f . f where f = reverse . dropWhile isSpace
           in Just (trim name ++ " :: " ++ trim rest)
        _ -> Nothing

-- 型シグネチャの断片を結合する（括弧の対応で判断）
mergeTypeLines :: [String] -> [String]
mergeTypeLines = go [] []
  where
    go acc [] [] = reverse acc
    go acc current [] = reverse (unlines current : acc)
    go acc current (l : ls)
      | null current && "::" `isInfixOf` l =
          let newCurrent = [l]
           in if balanced newCurrent
                then go (unlines newCurrent : acc) [] ls
                else go acc newCurrent ls
      | not (null current) =
          let newCurrent = current ++ [l]
           in if balanced newCurrent
                then go (unlines newCurrent : acc) [] ls
                else go acc newCurrent ls
      | otherwise = go acc current ls

    balanced ls =
      let s = concat ls
          count c = length . filter (== c)
       in count '(' s == count ')' s
            && count '[' s == count ']' s
            && count '{' s == count '}' s

{-}
extractTypeSigs :: [String] -> [String]
extractTypeSigs = go [] []
  where
    go acc current [] =
      case current of
        [] -> reverse acc
        _ -> reverse (unlines current : acc)
    go acc current (l : ls)
      | null current && "::" `isInfixOf` l =
          let newCurrent = [l]
           in if balanced newCurrent
                then go (unlines newCurrent : acc) [] ls
                else go acc newCurrent ls
      | not (null current) =
          let newCurrent = current ++ [l]
           in if balanced newCurrent
                then go (unlines newCurrent : acc) [] ls
                else go acc newCurrent ls
      | otherwise = go acc current ls

    balanced :: [String] -> Bool
    balanced ls =
      let s = concat ls
          count c = length . filter (== c)
          parens = (count '(' s, count ')' s)
          brackets = (count '[' s, count ']' s)
          braces = (count '{' s, count '}' s)
       in parens == swap parens
            && brackets == swap brackets
            && braces == swap braces

    swap (a, b) = (b, a)
-}
