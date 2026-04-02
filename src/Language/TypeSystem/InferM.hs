module Language.TypeSystem.InferM
  ( InferM,
    runInferWithDataEnv,
    InferState (..),
    emptyInferState,
    freshTVar,
    freshTypeVar,
    addPred,
    gets,
    modify,
    DataEnv,
    -- 他に必要な関数や型
  )
where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Language.TypeSystem.BaseType
-- import Language.TypeSystem.Class
import Language.TypeSystem.Error
-- import Language.TypeSystem.InferInstances
import Language.TypeSystem.Syntax

-- | 構築子の型情報
type DataEnv = Map.Map Name Scheme

-- | 推論モナドの状態
data InferState = InferState
  { count :: Int,
    constraints :: [Pred],
    dataEnv :: DataEnv
  }

-- | 推論モナド
type InferM = ExceptT InferError (State InferState)

-- | 初期状態
emptyInferState :: InferState
emptyInferState =
  InferState
    { count = 0,
      constraints = [],
      dataEnv = Map.empty
    }

freshTVar :: InferM Type
freshTVar = do
  s <- get
  let n = count s
  put s {count = n + 1}
  return $ TVar ("t" ++ show n)

-- | 型変数の生成
freshTypeVar :: InferM Type
freshTypeVar = do
  st <- get
  let n = count st
  put st {count = n + 1}
  return $ TVar ("a" ++ show n)

-- | 制約を追加
addPred :: Pred -> InferM ()
addPred p = modify $ \st -> st {constraints = p : constraints st}

-- | 推論を実行
runInferWithDataEnv :: DataEnv -> InferM a -> Either InferError a
runInferWithDataEnv denv m =
  evalState (runExceptT m) (emptyInferState {dataEnv = denv})
