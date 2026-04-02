module Language.TypeSystem.Env
  ( TypeEnv (..),
    emptyEnv,
    combineEnvs,
    singletonEnv,
    unionEnvs,
    getEnvMap,
  )
where

import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.Syntax

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

combineEnvs :: [TypeEnv] -> TypeEnv
combineEnvs = TypeEnv . Map.unions . map getEnvMap

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

singletonEnv :: Name -> Scheme -> TypeEnv
singletonEnv name scheme = TypeEnv (Map.singleton name scheme)

unionEnvs :: TypeEnv -> TypeEnv -> TypeEnv
unionEnvs (TypeEnv m1) (TypeEnv m2) = TypeEnv (Map.union m1 m2)

getEnvMap :: TypeEnv -> Map.Map Name Scheme
getEnvMap (TypeEnv m) = m
