module Language.TypeSystem.DeclDef where

import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Language.TypeSystem.Subst (Subst)
import Language.TypeSystem.BaseType
import Language.TypeSystem.Class
import Language.TypeSystem.ClassDef
import Language.TypeSystem.Decl
import Language.TypeSystem.Env
import Language.TypeSystem.InferM
import Language.TypeSystem.Syntax

class HasType a where
  -- | その要素から型を推論し、必要なら環境を更新する
  infer :: a -> InferM Type

class Dependency a where
  -- | その宣言が依存している（参照している）変数名のセットを返す
  freeVars :: a -> Set.Set Name