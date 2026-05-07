{-# LANGUAGE FlexibleContexts #-}

module Language.TypeSystem.Infer.Decl
  ( inferDecl,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Combinators (empty)
import Control.Monad.Except (throwError)
import Data.IntMap (null)
import Data.IntMap.Lazy (empty)
import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.Class
import Language.TypeSystem.ClassEnv
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.DeclInstance
import Language.TypeSystem.Env
import Language.TypeSystem.EnvInstance
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
-- import Language.TypeSystem.Generalize
import Language.TypeSystem.Infer.Expr
import Language.TypeSystem.Infer.Subst
import Language.TypeSystem.Infer.Unify
import Language.TypeSystem.InferM
import Language.TypeSystem.Utils.MyTrace
import Language.TypeSystem.Pattern
import qualified Language.TypeSystem.Pattern as TP
import Language.TypeSystem.PatternInfer
import Language.TypeSystem.Syntax

inferDecl :: Decl -> InferM (Subst, [Pred], TypeEnv)
inferDecl decl = case decl of
  DeclValue {} -> inferValueDecl decl
  DeclFunGroup name clauses -> inferValueDecl decl
  DeclTypeSig name sch -> return (nullSubst, [], singletonEnv name sch)
  other -> throwError $ OtherError $ "inferDecl: unsupported declaration: " ++ show other

inferValueDecl :: Decl -> InferM (Subst, [Pred], TypeEnv)
inferValueDecl decl = case decl of
  DeclFunGroup name clauses -> do
    (s, ps, ty) <- inferFunGroupDecl name clauses
    env <- getEnv
    let sch = generalize env ps ty
    checkPredicates initialClassEnv (applySubst s ps) -- ← ここで制約をチェック！
    return (s, ps, singletonEnv name sch)
  DeclTypeSig name sch ->
    return (emptySubst, [], singletonEnv name sch)
  DeclValue pat expr -> do
    (s1, ps1, tExpr) <- inferExpr expr
    (s2, envPat, tPat) <- inferPattern pat
    s3 <- unify (applySubst s2 tExpr) tPat
    let s = s3 `composeSubst` s2 `composeSubst` s1
        ps = applySubst s ps1
    env0 <- getEnv
    let env' = applySubst s env0
        envPat' = applySubst s envPat
    let TypeEnv envMap = envPat'
        generalizedEnv =
          Map.map (\(Forall _ _ ty) -> generalize env' ps ty) envMap
    return (s, ps, TypeEnv generalizedEnv)
  {-}
    DeclValue pat expr -> do
      (s1, ps1, tExpr) <- inferExpr expr
      (s2, ps2, tPat, envPat) <- inferPattern pat
      s3 <- unify (applySubst s2 tExpr) tPat
      let s = s3 `composeSubst` s2 `composeSubst` s1
      env <- getEnv
      let schEnv = generalize (applySubst s env) (applySubst s (ps1 ++ ps2)) (applySubst s tPat)
      return (s, applySubst s (ps1 ++ ps2), envPat)
  -}
  _ -> throwError $ OtherError $ "inferValueDecl: unsupported declaration: " ++ show decl

inferFunGroupDecl :: Name -> [FunClause] -> InferM (Subst, [Pred], Type)
inferFunGroupDecl name clauses = do
  let exprs = map clauseToExpr clauses
  results <- mapM inferExpr exprs
  let substs = map (\(s, _, _) -> s) results
      preds = concatMap (\(_, ps, _) -> ps) results
      types = map (\(_, _, t) -> t) results
  t <- freshTypeVar
  sUnify <-
    foldM
      ( \s ty -> do
          s' <- unify (applySubst s ty) t
          return (s' `composeSubst` s)
      )
      nullSubst
      types
  let finalSubst = foldr composeSubst sUnify substs
      finalType = applySubst finalSubst t
  return (finalSubst, applySubst finalSubst preds, finalType)

clauseToExpr (FunClause pats _ (Just body) (Just wheres)) =
  foldr ELam (EWhere body (map convertDecl wheres)) [pats]
clauseToExpr (FunClause pats Nothing (Just body) Nothing) =
  foldr ELam body [pats]
clauseToExpr (FunClause _ _ _ (Just _)) =
  error "where declarations not yet supported"

checkPredicates :: ClassEnv -> [Pred] -> InferM ()
checkPredicates env preds = mapM_ check preds
  where
    check (IsIn cls ty) =
      case ty of
        TVar _ -> return ()
        _ ->
          if isValidInstance env cls ty
            then return ()
            else throwError $ UnificationFail ty (TCon ("<" ++ cls ++ " instance>"))

convertDecl :: Decl -> (Pattern, Expr)
convertDecl (DeclValue pat expr) = (pat, expr)
convertDecl d =
  error $ "convertDecl: unsupported declaration in where: " ++ show d
