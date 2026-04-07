module Language.TypeSystem.DataEnv
  ( DataEnv,
    preludeDataEnv,
    lookupConstructor,
  )
where

import Control.Monad.State
import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.Error
import Language.TypeSystem.InferM
import Language.TypeSystem.Syntax

{-}
-- | 構築子名から型スキームへのマップ
type DataEnv = Map.Map Name Scheme

data InferState = InferState
  { count :: Int,
    constraints :: [Pred],
    dataEnv :: DataEnv
  }

emptyInferState :: InferState
emptyInferState =
  InferState
    { count = 0,
      constraints = [],
      dataEnv = Map.empty
    }
-}

lookupConstructor :: Name -> InferM (Maybe Scheme)
lookupConstructor name = do
  env <- gets dataEnv
  return $ Map.lookup name env

-- | 標準の構築子の型情報（Maybe, Bool, List など）
preludeDataEnv :: DataEnv
preludeDataEnv =
  Map.fromList
    [ ("Just", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "a")))),
      ("Nothing", Forall ["a"] [] (TApp (TCon "Maybe") (TVar "a"))),
      ("True", Forall [] [] (TCon "Bool")),
      ("False", Forall [] [] (TCon "Bool")),
      (":", Forall ["a"] [] (TArrow (TVar "a") (TArrow (TList (TVar "a")) (TList (TVar "a"))))),
      ("[]", Forall ["a"] [] (TList (TVar "a")))
    ]
