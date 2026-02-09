module TypeInference.Error where

import AST.Type (Type (..))
import TypeInference.Unify (UnifyError)

-- 型推論全体で使うエラー型
data InferError
  = InferUnboundVariable String
  | InferMismatch Type Type
  | InferUnifyError UnifyError
  | InferOther String
  deriving (Show, Eq)
