module Language.TypeSystem.SchemeInstance where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.Class
import Language.TypeSystem.InferM
import Language.TypeSystem.Infer.Subst (Subst)
import qualified Language.TypeSystem.Infer.Subst as SB
import Language.TypeSystem.Syntax

-- | Scheme に対する SchemeLike インスタンス
instance SchemeLike Scheme where
  instantiate (Forall vars preds t) = do
    freshVars <- mapM (const freshTypeVar) vars
    let s = Map.fromList (zip vars freshVars)
    return $ SB.applySubst s t

  generalize env t =
    let ftvEnv = freeTypeVars env
        ftvT = freeTypeVars t
        vars = Set.toList (ftvT `Set.difference` ftvEnv)
     in Forall vars [] t -- Predは後で拡張
