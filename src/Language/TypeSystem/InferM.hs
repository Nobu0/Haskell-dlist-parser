{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.TypeSystem.InferM
  ( InferM,
    runInferWithDataEnv,
    InferState (..),
    emptyInferState,
    freshTVar,
    freshTypeVar,
    freshName,
    addPred,
    gets,
    getEnv,
    putEnv,
    modify,
    DataEnv,
    extendEnvWithPattern,
    localEnv,
    extendEnvRaw,
    mergeEnvs,
    -- initialEnv,
    evalInferM,
    emptyInferState,
    -- 他に必要な関数や型
  )
where

-- import Language.TypeSystem.Class

-- import Language.TypeSystem.InferInstances

-- import Language.TypeSystem.EnvInstance

import Control.Applicative (Alternative (empty))
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Language.TypeSystem.BaseType
-- import Language.TypeSystem.DataEnv (preludeDataEnv)
import Language.TypeSystem.Env
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
import Language.TypeSystem.Pattern
import Language.TypeSystem.Prelude
import Language.TypeSystem.Syntax

-- | 構築子の型情報
type DataEnv = Map.Map Name Scheme

-- | 推論モナドの状態
data InferState = InferState
  { count :: Int,
    constraints :: [Pred],
    dataEnv :: DataEnv,
    env :: TypeEnv
  }

{-}
-- | 推論モナド
type InferM = ExceptT InferError (State InferState)
-}
newtype InferM a = InferM {runInferM :: ExceptT InferError (StateT InferState IO) a}
  deriving (Functor, Applicative, Monad, MonadError InferError, MonadState InferState)

-- newtype InferM a = InferM {runInferM :: ExceptT InferError (State InferState) a}

instance MonadIO InferM where
  liftIO io = InferM (lift (lift io))

evalInferM :: TypeEnv -> InferM a -> IO (Either InferError a)
evalInferM env (InferM m) =
  evalStateT (runExceptT m) (emptyInferState {env = env})

extendTypeEnv :: Name -> Scheme -> TypeEnv -> TypeEnv
extendTypeEnv name scheme (TypeEnv env) = TypeEnv (Map.insert name scheme env)

localEnv :: TypeEnv -> InferM a -> InferM a
localEnv env' action = do
  st <- get
  put st {env = env'}
  result <- action
  modify (\s -> s {env = env st}) -- 元に戻す
  return result

extendEnvWithPattern :: Pattern -> Scheme -> InferM a -> InferM a
extendEnvWithPattern (PVar name) scheme action = do
  oldEnv <- getEnv
  let newEnv = extendTypeEnv name scheme oldEnv
  localEnv newEnv action
extendEnvWithPattern _ _ _ =
  throwError $ OtherError "Only simple variable patterns are supported in extendEnvWithPattern"

getEnv :: InferM TypeEnv
getEnv = gets env

putEnv :: TypeEnv -> InferM ()
putEnv e = modify (\st -> st {env = e})

extendEnvRaw :: TypeEnv -> InferM ()
extendEnvRaw newEnv = modify (\st -> st {env = newEnv `mergeEnvs` env st})

mergeEnvs :: TypeEnv -> TypeEnv -> TypeEnv
mergeEnvs (TypeEnv new) (TypeEnv old) = TypeEnv (Map.union new old)

-- | 初期状態
emptyInferState :: InferState
emptyInferState =
  InferState
    { count = 0,
      constraints = [],
      dataEnv = preludeDataEnv,
      env = initialEnv
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

freshId :: InferM Int
freshId = do
  st <- get
  let n = count st
  put st {count = n + 1}
  return n

freshName :: InferM Name
freshName = do
  n <- freshId
  return ("x" ++ show n)

-- | 制約を追加
addPred :: Pred -> InferM ()
addPred p = modify $ \st -> st {constraints = p : constraints st}

runInferWithDataEnv :: DataEnv -> InferM a -> IO (Either InferError a)
runInferWithDataEnv denv (InferM m) =
  evalStateT (runExceptT m) (emptyInferState {dataEnv = denv})
