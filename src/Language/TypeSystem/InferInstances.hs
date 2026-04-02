-- Language.TypeSystem.InferInstances

module Language.TypeSystem.InferInstances where

import qualified Data.Map as Map
import Language.TypeSystem.ClassDef
import Language.TypeSystem.InferM
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax

instance SchemeLike Scheme where
  instantiate (Forall vars preds t) = do
    newVars <- mapM (const freshTVar) vars
    let s = Map.fromList (zip vars newVars)
        t' = applySubst s t
        preds' = map (applySubst s) preds
    mapM_ addPred preds'
    return t'
