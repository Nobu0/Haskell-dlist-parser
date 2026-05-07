module Language.TypeSystem.Utils.Utils (simplifyPreds) where

import qualified Data.Set as Set
-- import Language.TypeSystem.BaseType -- (Pred(..), Type(..))
-- import Language.TypeSystem.Class -- (Pred(..))
import Language.TypeSystem.Syntax

-- | 自明な制約を除外し、重複を取り除く
simplifyPreds :: [Pred] -> [Pred]
simplifyPreds preds =
  let unique = Set.toList (Set.fromList preds)
   in filter (not . isTriv) unique

-- | 自明な制約（たとえば Num Int など）を除外
isTriv :: Pred -> Bool
isTriv (IsIn _ (TCon _)) = True -- Int, Bool などの既知の型は省略
isTriv _ = False
