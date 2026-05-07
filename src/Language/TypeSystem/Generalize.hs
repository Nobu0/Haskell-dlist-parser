module Language.TypeSystem.Generalize
  ( generalizeType,
    instantiateScheme,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.Class
import Language.TypeSystem.Env
import Language.TypeSystem.InferM
import Language.TypeSystem.Infer.Subst
import qualified Language.TypeSystem.Infer.Subst as SB
import Language.TypeSystem.Syntax

-- | 型をスキームに一般化する（環境に現れない自由変数を量化）
generalizeType :: TypeEnv -> [Pred] -> Type -> Scheme
generalizeType env preds t =
  let ftvEnv = freeTypeVars env
      ftvAll = freeTypeVars t `Set.union` foldMap freeTypeVars preds
      vars = Set.toList (ftvAll `Set.difference` ftvEnv)
   in Forall vars preds t

-- | 型スキームを具体的な型にインスタンス化する
instantiateScheme :: Scheme -> InferM Type
instantiateScheme (Forall vars preds t) = do
  freshVars <- mapM (const freshTypeVar) vars
  let s = Map.fromList (zip vars freshVars)
  -- addPreds (map (SB.applySubst s) preds)
  mapM_ (addPred . SB.applySubst s) preds
  return $ SB.applySubst s t
