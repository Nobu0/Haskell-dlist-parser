module TypeInference.Subst
  ( Subst,
    emptySubst,
    singletonSubst,
    composeSubst,
    apply,
    applyConstraint,
  )
where

import qualified Data.Map as Map
import TypeInference.Type

type Subst = Map.Map String Type

-- 空の代入
emptySubst :: Subst
emptySubst = Map.empty

-- 単一代入
singletonSubst :: String -> Type -> Subst
singletonSubst = Map.singleton

-- 代入の合成
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 =
  Map.map (apply s1) s2 `Map.union` s1

-- 型への代入適用
apply :: Subst -> Type -> Type
apply s t = case t of
  TVar v ->
    case Map.lookup v s of
      Just t' -> t'
      Nothing -> TVar v
  TCon c -> TCon c
  TArrow t1 t2 -> TArrow (apply s t1) (apply s t2)
  TList t1 -> TList (apply s t1)
  TApp t1 t2 -> TApp (apply s t1) (apply s t2)
  TConstraint cs t1 -> TConstraint (map (applyConstraint s) cs) (apply s t1)
  TForall vars t1 ->
    let s' = foldr Map.delete s vars
     in TForall vars (apply s' t1)
  TTuple ts -> TTuple (map (apply s) ts)
  TUnit -> TUnit
  TRecord fields -> TRecord (Map.map (apply s) fields)

-- 制約への代入適用
applyConstraint :: Subst -> Constraint -> Constraint
applyConstraint s (Constraint cls ts) =
  Constraint cls (map (apply s) ts)
