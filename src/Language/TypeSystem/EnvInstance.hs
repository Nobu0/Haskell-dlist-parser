module Language.TypeSystem.EnvInstance where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.Class
import Language.TypeSystem.Env
import Language.TypeSystem.Subst
import qualified Language.TypeSystem.Subst as SB
import Language.TypeSystem.Syntax

instance EnvLike TypeEnv where
  lookupEnv x (TypeEnv env) = Map.lookup x env
  extendEnv x s (TypeEnv env) = TypeEnv (Map.insert x s env)
  freeTypeVarsEnv (TypeEnv env) = Set.unions (map freeTypeVars (Map.elems env))
  applySubstEnv s (TypeEnv env) = TypeEnv (Map.map (applySubst s) env)

{-}
-- | TypeEnv に対する EnvLike インスタンス
instance EnvLike TypeEnv where
  lookupEnv = Map.lookup

  extendEnv = Map.insert

  freeTypeVarsEnv env =
    foldMap freeTypeVars (Map.elems env)

  applySubstEnv s =
    Map.map (SB.applySubst s)
-}