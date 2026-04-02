{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TypeInference.Types where

import qualified Data.Map as M
import TypeInference.Type -- あなたの Type や Scheme の定義があるモジュール

newtype TypeEnv = TypeEnv (M.Map Name Scheme)
  deriving (Eq, Show)

-- 型スキーム
data Scheme = Forall [Name] Type
  deriving (Eq, Show)

-- 型環境
-- type TypeEnv = M.Map Name Scheme

-- 置換
type Subst = M.Map Name Type

-- 型クラス定義
class Types a where
  ftv :: a -> [Name]
  apply :: Subst -> a -> a

-- Type に対するインスタンス
instance Types Type where
  ftv :: Type -> [Name]
  ftv (TVar n) = [n]
  ftv (TCon _) = []
  ftv (TArrow t1 t2) = ftv t1 ++ ftv t2
  ftv (TList t) = ftv t
  ftv (TApp t1 t2) = ftv t1 ++ ftv t2
  ftv (TConstraint _ t) = ftv t
  ftv (TForall vars t) = filter (`notElem` vars) (ftv t)
  ftv (TTuple ts) = concatMap ftv ts
  ftv TUnit = []

  apply :: Subst -> Type -> Type
  apply s (TVar n) = case M.lookup n s of
    Just t -> t
    Nothing -> TVar n
  apply _ (TCon c) = TCon c
  apply s (TArrow t1 t2) = TArrow (apply s t1) (apply s t2)
  apply s (TList t) = TList (apply s t)
  apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)
  apply s (TConstraint cs t) = TConstraint cs (apply s t)
  apply s (TForall vars t) =
    let s' = foldr M.delete s vars
     in TForall vars (apply s' t)
  apply s (TTuple ts) = TTuple (map (apply s) ts)
  apply _ TUnit = TUnit

-- Scheme に対するインスタンス
instance Types Scheme where
  ftv :: Scheme -> [Name]
  ftv (Forall vars t) = filter (`notElem` vars) (ftv t)
  apply :: Subst -> Scheme -> Scheme
  apply s (Forall vars t) =
    let s' = foldr M.delete s vars
     in Forall vars (apply s' t)

-- TypeEnv に対するインスタンス
{-}
instance Types TypeEnv where
  ftv env = concatMap ftv (M.elems env)
  apply s env = M.map (apply s) env
-}
instance Types TypeEnv where
  ftv :: TypeEnv -> [Name]
  ftv (TypeEnv env) = concatMap ftv (M.elems env)
  apply :: Subst -> TypeEnv -> TypeEnv
  apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)
