module Language.TypeSystem.Env
  ( TypeEnv (..),
    emptyEnv,
    combineEnvs,
    singletonEnv,
    unionEnvs,
    getEnvMap,
    envMerge,
    -- initialEnv,
  )
where

import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.Syntax

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

envMerge :: TypeEnv -> TypeEnv -> TypeEnv
envMerge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

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

-- extendEnv :: Name -> Scheme -> TypeEnv -> TypeEnv
-- extendEnv name scheme (TypeEnv env) = TypeEnv (Map.insert name scheme env)

-- lookupEnv :: Name -> TypeEnv -> Maybe Scheme
-- lookupEnv name (TypeEnv env) = Map.lookup name env

-- applySubstEnv :: Subst -> TypeEnv -> TypeEnv
-- applySubstEnv s (TypeEnv env) = TypeEnv (Map.map (applySubst s) env)
