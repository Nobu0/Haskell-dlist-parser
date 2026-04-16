{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.TypeSystem.Class
  ( Types (..),
    SchemeLike (..),
    EnvLike (..),
    ConstraintLike (..),
    freeTypeVars,
    applySubstToAlt,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.BaseType
import Language.TypeSystem.ClassDef
import Language.TypeSystem.Env
import Language.TypeSystem.Expr
import Language.TypeSystem.InferM
import Language.TypeSystem.Pattern
import Language.TypeSystem.Syntax


freeTypeVars :: (Types a) => a -> Set.Set Name
freeTypeVars = ftv

-- | Type への適用
instance Types Type where
  applySubst s t = case t of
    TVar n -> Map.findWithDefault t n s
    TCon _ -> t
    TApp l r -> TApp (applySubst s l) (applySubst s r)
    TArrow a b -> TArrow (applySubst s a) (applySubst s b)
    TTuple ts -> TTuple (map (applySubst s) ts)
    TList t1 -> TList (applySubst s t1)
    TRecord fields Nothing -> TRecord (Map.map (applySubst s) fields) Nothing
    TRecord fields (Just row) -> TRecord (Map.map (applySubst s) fields) (Just (applySubst s row))
    expr -> expr -- catch-all fallback（開発中のみ）

  ftv t = case t of
    TVar n -> Set.singleton n
    TCon _ -> Set.empty
    TApp l r -> ftv l `Set.union` ftv r
    TArrow a b -> ftv a `Set.union` ftv b
    TTuple ts -> Set.unions (map ftv ts)
    TList t1 -> ftv t1
    TRecord fields Nothing -> Set.unions (map ftv (Map.elems fields))
    TRecord fields (Just row) -> Set.unions (map ftv (Map.elems fields) ++ [ftv row])

-- | Scheme への適用（束縛変数は無視）
instance Types Scheme where
  applySubst s (Forall vars preds t) =
    let s' = foldr Map.delete s vars
     in Forall vars (map (applySubst s') preds) (applySubst s' t)

  ftv (Forall vars preds t) =
    (ftv preds `Set.union` ftv t) `Set.difference` Set.fromList vars

-- | Pred への適用
instance Types Pred where
  applySubst s (IsIn cls t) = IsIn cls (applySubst s t)
  ftv (IsIn _ t) = ftv t

-- | リストへの適用
instance (Types a) => Types [a] where
  applySubst s = map (applySubst s)
  ftv = foldr (Set.union . ftv) Set.empty

-- | 型環境への適用
instance Types TypeEnv where
  applySubst s (TypeEnv env) = TypeEnv (Map.map (applySubst s) env)
  ftv (TypeEnv env) = ftv (Map.elems env)

instance Types Pattern where
  applySubst s (PVar name) = PVar name -- (applySubst s ty)
  applySubst _ PWildcard = PWildcard
  applySubst s (PTuple ps) = PTuple (map (applySubst s) ps)
  applySubst s (PCons name ps) = PCons name (applySubst s ps)

  ftv (PVar _) = Set.empty
  ftv PWildcard = Set.empty
  ftv (PTuple ps) = Set.unions (map ftv ps)
  ftv (PCons _ ps) = ftv ps

instance Types Expr where
  applySubst s (EVar x) = EVar x
  applySubst s (EInt l) = EInt l
  applySubst s (EBool b) = EBool b
  applySubst s (ELam x e) = ELam x (applySubst s e)
  applySubst s (EApp e1 e2) = EApp (applySubst s e1) (applySubst s e2)
  applySubst s (ELet x e1 e2) = ELet x (applySubst s e1) (applySubst s e2)
  applySubst s (ECase e alts) = ECase (applySubst s e) (map (applySubstToAlt s) alts)
  applySubst s (EList es) = EList (map (applySubst s) es)
  applySubst s (ELambdaCase alts) = ELambdaCase (map (applySubstToAlt s) alts)
  applySubst _ expr = expr -- catch-all fallback（開発中のみ）

  ftv (EVar _) = Set.empty
  ftv (EInt _) = Set.empty
  ftv (ELam _ e) = ftv e
  ftv (EApp e1 e2) = ftv e1 `Set.union` ftv e2
  ftv (ELet _ e1 e2) = ftv e1 `Set.union` ftv e2
  ftv (ECase e alts) = ftv e `Set.union` Set.unions (map ftv alts)
  ftv (EList es) = Set.unions (map ftv es)

applySubstToAlt :: Subst -> CaseAlt -> CaseAlt
{-}
applySubstToAlt s (CaseAlt pat expr) = CaseAlt (applySubst s pat) (applySubst s expr)
applySubstToAlt s (CaseAltGuard pat guards) =
  CaseAltGuard (applySubst s pat) [(applySubst s c, applySubst s e) | (c, e) <- guards]
-}
applySubstToAlt s alt = case alt of
  CaseAlt pat expr -> CaseAlt (applySubst s pat) (applySubst s expr)
  CaseAltGuard pat guards -> CaseAltGuard (applySubst s pat) [(applySubst s c, applySubst s e) | (c, e) <- guards]

instance Types CaseAlt where
  applySubst s (CaseAlt pat expr) = CaseAlt pat (applySubst s expr)
  applySubst s (CaseAltGuard pat guards) =
    CaseAltGuard pat [(applySubst s cond, applySubst s body) | (cond, body) <- guards]

  ftv (CaseAlt _ expr) = ftv expr
  ftv (CaseAltGuard _ guards) = Set.unions [ftv cond `Set.union` ftv body | (cond, body) <- guards]

instance Types (Map.Map Name Type) where
  applySubst s = Map.map (applySubst s)
  ftv = foldMap ftv . Map.elems
