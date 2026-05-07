-- Language.TypeSystem.InferInstances

module Language.TypeSystem.InferInstances where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.ClassDef
import Language.TypeSystem.InferM
import Language.TypeSystem.Infer.Subst
import Language.TypeSystem.Syntax

{-}
instance SchemeLike Scheme where
  instantiate (Forall vars preds t) = do
    newVars <- mapM (const freshTVar) vars
    let s = Map.fromList (zip vars newVars)
        t' = applySubst s t
        preds' = map (applySubst s) preds
    mapM_ addPred preds'
    return t'
-}
instance SchemeLike Scheme where
  instantiate (Forall vars preds t) = do
    tvs <- mapM (const freshTypeVar) vars
    let s = Map.fromList (zip vars tvs)
    return (applySubst s t, applySubst s preds)

  generalize env preds t =
    let vars = Set.toList $ ftv t Set.\\ ftv env
     in Forall vars preds t

{-}
instance SchemeLike Scheme where
  instantiate (Forall vars preds t) = do
    tvs <- mapM (const freshTypeVar) vars
    let s = Map.fromList (zip vars tvs)
    return (applySubst s t, applySubst s preds)

    generalize env preds t =
      let vars = ftv t \\ ftv env
      in Forall vars preds t
-}
{-}
  generalize env t =
    let vars = ftv t `Set.difference` ftv env
     in Forall (Set.toList vars) [] t

-- generalize env t = generalizeScheme env t

instance SchemeLike Scheme where
  instantiate (Forall vars preds t) = do
    newVars <- mapM (const freshTVar) vars
    let s = Map.fromList (zip vars newVars)
        t' = applySubst s t
        preds' = map (applySubst s) preds
    mapM_ addPred preds'
    return t'

  generalize env t =
    let vars = ftv t `Set.difference` ftv env
     in Forall (Set.toList vars) [] t
-}
{-}
  generalize (TypeEnv env) t =
    let vars = ftv t `Set.difference` ftv env
    in Forall (Set.toList vars) [] t
generalize :: TypeEnv -> Type -> Scheme
generalize (TypeEnv env) t =
  let vars = ftv t `Set.difference` ftv env
   in Forall (Set.toList vars) [] t
generalize :: TypeEnv -> Type -> Scheme
generalize (TypeEnv env) t =
  let vars = ftv t `Set.difference` ftv env
  in Forall (Set.toList vars) [] t
-}
