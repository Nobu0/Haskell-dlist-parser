module Language.TypeSystem.ClassDef where

import qualified Data.Set as Set
-- import Language.TypeSystem.Subst (Subst)
import Language.TypeSystem.BaseType
import Language.TypeSystem.Env
import Language.TypeSystem.InferM
import Language.TypeSystem.Syntax

class Types a where
  applySubst :: Subst -> a -> a
  ftv :: a -> Set.Set Name

{-}
class SchemeLike scheme where
  instantiate :: scheme -> InferM Type
  generalize :: TypeEnv -> Type -> scheme
-}
class SchemeLike scheme where
  instantiate :: scheme -> InferM (Type, [Pred])
  generalize :: TypeEnv -> [Pred] -> Type -> scheme

{-}
class SchemeLike scheme where
  instantiate :: scheme -> InferM (Type, [Pred])

  -- generalize :: TypeEnv -> Type -> scheme
  generalize :: TypeEnv -> [Pred] -> Type -> Scheme
-}
class ConstraintLike c where
  unifyConstraint :: c -> InferM Subst

class EnvLike env where
  lookupEnv :: Name -> env -> Maybe Scheme
  extendEnv :: Name -> Scheme -> env -> env
  freeTypeVarsEnv :: env -> Set.Set Name
  applySubstEnv :: Subst -> env -> env
