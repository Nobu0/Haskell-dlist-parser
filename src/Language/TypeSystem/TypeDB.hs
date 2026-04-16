module Language.TypeSystem.TypeDB
  ( TypeDB (..),
    loadTypeDB,
    {-}
        emptyTypeDB,
        insertTypeSig,
        insertInstance,
        lookupTypeSig,
        lookupInstances,
        -- mergeTypeDB,
    -}
  )
where

import qualified Data.Map as M
import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.ClassEnv
import Language.TypeSystem.Syntax
import System.IO

type TypeDB = M.Map String String

loadTypeDB :: FilePath -> IO TypeDB
loadTypeDB path = do
  contents <- readFile path
  let ls = lines contents
      ls' = drop 1 ls -- ヘッダ行をスキップ
      pairs = map parseLine ls'
  return (M.fromList pairs)

parseLine :: String -> (String, String)
parseLine line =
  case break (== '\t') line of
    (name, '\t' : typ) -> (name, typ)
    _ -> error ("Invalid line in typeDB: " ++ line)

-- type ModuleName = String

-- type Name = String
{-}
data TypeDB = TypeDB
  { typeSigs :: Map.Map (ModuleName, Name) Scheme,
    classEnv :: ClassEnv
  }
  deriving (Show)

emptyTypeDB :: TypeDB
emptyTypeDB =
  TypeDB
    { typeSigs = Map.empty,
      classEnv = initialClassEnv
    }

insertTypeSig :: ModuleName -> Name -> Scheme -> TypeDB -> TypeDB
insertTypeSig mod name sig db =
  db {typeSigs = Map.insert (mod, name) sig (typeSigs db)}

lookupTypeSig :: ModuleName -> Name -> TypeDB -> Maybe Scheme
lookupTypeSig mod name db = Map.lookup (mod, name) (typeSigs db)

insertInstance :: ClassName -> TypeName -> TypeDB -> TypeDB
insertInstance cls ty db =
  db {classEnv = addInstance cls ty (classEnv db)}

lookupInstances :: ClassName -> TypeDB -> [TypeName]
lookupInstances cls db =
  case Map.lookup cls (classes (classEnv db)) of
    Just tys -> tys
    Nothing -> []
-}
