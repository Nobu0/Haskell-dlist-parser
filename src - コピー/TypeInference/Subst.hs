module TypeInference.Subst
  ( Subst,
    emptySubst,
    singletonSubst,
    composeSubst,
    apply,
  )
where

import AST.Type
import qualified Data.Map as M

-- 型代入：型変数名 → 型
type Subst = M.Map String Type

-- 空の代入
emptySubst :: Subst
emptySubst = M.empty

-- 単一代入
singletonSubst :: String -> Type -> Subst
singletonSubst = M.singleton

-- 代入の合成
-- s1 を先に適用し、その結果に s2 を適用する
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 =
  M.map (apply s1) s2 `M.union` s1

-- 型への代入適用
apply :: Subst -> Type -> Type
apply s t = case t of
  TUnit ->
    TUnit
  TVar v ->
    case M.lookup v s of
      Just t' -> t'
      Nothing -> TVar v
  TCon c ->
    TCon c
  TArrow t1 t2 ->
    TArrow (apply s t1) (apply s t2)
  TList t1 ->
    TList (apply s t1)
  TApp t1 t2 ->
    TApp (apply s t1) (apply s t2)
  TConstraint cs t1 ->
    TConstraint (map (applyConstraint s) cs) (apply s t1)
  TForall vars t1 ->
    -- Forall の束縛変数には代入を適用しない
    let s' = foldr M.delete s vars
     in TForall vars (apply s' t1)
  TTuple ts ->
    TTuple (map (apply s) ts)

-- 制約への代入適用
applyConstraint :: Subst -> Constraint -> Constraint
applyConstraint s (Constraint cls ts) =
  Constraint cls (map (apply s) ts)
