module Language.TypeSystem.Error where

import Language.TypeSystem.BaseType
import Language.TypeSystem.Syntax

-- | 型推論中に発生しうるエラー
data InferError
  = UnboundVariable Name
  | UnificationFail Type Type
  | UnificationMismatch Type Type
  | InfiniteType TVar Type
  | Ambiguous [Pred]
  | UnificationError String
  | OtherError String
  deriving (Eq, Show)

prettyInferError :: InferError -> String
prettyInferError err = case err of
  UnboundVariable name ->
    "Unbound variable: " ++ name
  UnificationFail t1 t2 ->
    "Cannot unify types: " ++ show t1 ++ " ~ " ++ show t2
  UnificationMismatch t1 t2 ->
    "Type mismatch: " ++ show t1 ++ " vs " ++ show t2
  InfiniteType v t ->
    "Infinite type: " ++ v ++ " occurs in " ++ show t
  Ambiguous preds ->
    "Ambiguous constraints: " ++ unwords (map show preds)
  UnificationError msg ->
    "Unification error: " ++ msg
  OtherError msg ->
    "Error: " ++ msg
