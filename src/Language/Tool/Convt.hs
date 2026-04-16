module Language.Tool.Convt(
  loadTypeDBFromJson
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Language.Tool.Type
import qualified Language.Tool.Type as T
import Language.TypeSystem.BaseType
import Language.TypeSystem.ClassEnv
import qualified Language.TypeSystem.ClassEnv as CE
import Language.TypeSystem.Syntax
import Language.TypeSystem.TypeDB

-- 例：Language.TypeSystem.ClassEnv にて
addClassEnv :: String -> [String] -> ClassEnv -> ClassEnv
addClassEnv cls _ env = env {CE.classes = Map.insert cls [] (CE.classes env)}

loadTypeDBFromJson :: ModuleName -> FilePath -> IO TypeDB
loadTypeDBFromJson modName path = do
  content <- BL.readFile path
  case decode content of
    Just parsed -> return $ buildTypeDB modName parsed
    Nothing -> error "Failed to parse JSON"

buildTypeDB :: ModuleName -> TypeDBJson -> TypeDB
buildTypeDB modName (TypeDBJson tys cls insts) =
  TypeDB
    { typeSigs = Map.fromList (map (parseTypeSig modName) tys),
      classEnv = foldr addInsts (foldr addClass initialClassEnv cls) insts
    }

-- 仮のパーサ（本来はちゃんとした構文解析が必要）
parseTypeSig :: ModuleName -> String -> ((ModuleName, Name), Scheme)
parseTypeSig modName line =
  let (name, rest) = break (== ':') line
   in ((modName, name), dummyScheme rest)

dummyScheme :: String -> Scheme
dummyScheme _ = Forall [] [] (TVar "a") -- 仮の型スキーム（後でちゃんと実装）

addClass :: String -> ClassEnv -> ClassEnv
addClass line env =
  case words line of
    ("class" : cls : _) -> addClassEnv cls [] env
    _ -> env

addInsts :: String -> ClassEnv -> ClassEnv
addInsts line env =
  case words line of
    ("instance" : cls : ty : _) -> addInstance cls ty env
    _ -> env
