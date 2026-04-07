{-# LANGUAGE FlexibleContexts #-}

module Language.TypeSystem.InferDecl
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
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.Env
import Language.TypeSystem.EnvInstance
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
import Language.TypeSystem.Generalize
import Language.TypeSystem.InferExpr
import Language.TypeSystem.InferM
import Language.TypeSystem.MyTrace
import Language.TypeSystem.Pattern
import qualified Language.TypeSystem.Pattern as TP
import Language.TypeSystem.PatternInfer
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax
import Language.TypeSystem.Unify

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
    checkPredicates (applySubst s ps) -- ← ここで制約をチェック！
    return (s, ps, singletonEnv name sch)

  {-}
    DeclFunGroup name clauses -> do
      (s, ps, ty) <- inferFunGroupDecl name clauses
      env <- getEnv
      let sch = generalize env ps ty
      return (s, ps, singletonEnv name sch)
  -}
  DeclTypeSig name sch ->
    return (emptySubst, [], singletonEnv name sch)
  DeclValue pat expr -> do
    (s, ps, ty) <- inferExpr expr
    env <- getEnv
    let sch = generalize env ps ty
    case pat of
      PVar name -> return (s, ps, singletonEnv name sch)
      _ -> throwError $ OtherError "inferValueDecl: only simple variable patterns are supported for now"
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

clauseToExpr (FunClause pats Nothing (Just body) Nothing) =
  foldr ELam body [pats]
clauseToExpr (FunClause _ _ _ (Just _)) =
  error "where declarations not yet supported"

checkPredicates :: [Pred] -> InferM ()
checkPredicates preds = mapM_ check preds
  where
    check (IsIn cls ty) =
      case ty of
        TVar _ -> return () -- 型変数なら保留（OK）
        _ ->
          if isValidInstance cls ty
            then return ()
            else throwError $ UnificationFail ty (TCon ("<" ++ cls ++ " instance>"))

{-}
-- 与えられた制約が解決可能かチェックする
checkPredicates :: [Pred] -> InferM ()
checkPredicates preds = do
  mapM_ check preds
  where
    check (IsIn cls ty) = do
      if isValidInstance cls ty
        then return ()
        else throwError $ UnificationFail ty (TCon ("<" ++ cls ++ " instance>"))
-}

isValidInstance :: String -> Type -> Bool
isValidInstance "Num" (TCon "Int") = True
isValidInstance "Num" (TCon "Float") = True
isValidInstance "Num" (TCon "Double") = True
isValidInstance "Eq" (TCon "Int") = True
isValidInstance "Eq" (TCon "Bool") = True
isValidInstance _ _ = False

{-}
inferDecl :: Decl -> InferM (Subst, [Pred], TypeEnv)
inferDecl decl = case decl of
  DeclValue {} -> inferValueDecl decl
  DeclFunGroup name clauses -> do
    (s, ps, ty) <- inferFunGroupDecl name clauses
    env <- getEnv
    let sch = generalize env ps ty
    return (s, ps, singletonEnv name sch)
  DeclTypeSig name sch ->
    return (nullSubst, [], singletonEnv name sch)
  other ->
    throwError $ OtherError $ "inferDecl: unsupported declaration: " ++ show other

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

inferValueDecl :: Decl -> InferM (Subst, [Pred], TypeEnv)
inferValueDecl decl = case decl of
  DeclFunGroup name clauses -> do
    (s, ps, ty) <- inferFunGroupDecl name clauses
    env <- getEnv
    let sch = generalize env ps ty
    return (s, ps, singletonEnv name sch)
  DeclTypeSig name sch ->
    return (emptySubst, [], singletonEnv name sch)
  DeclValue pat expr -> do
    (s, ps, ty) <- inferExpr expr
    env <- getEnv
    let scheme = generalize env ps ty
    case pat of
      PVar name -> return (s, ps, singletonEnv name scheme)
      _ -> throwError $ OtherError "inferValueDecl: only simple variable patterns are supported for now"
  _ -> throwError $ OtherError $ "inferValueDecl: unsupported declaration: " ++ show decl
-}
{-}
inferValueDecl :: Decl -> InferM (Subst, [Pred], TypeEnv)
inferValueDecl decl = case decl of
  DeclFunGroup name clauses -> do
    (s, ps, ty) <- inferFunGroupDecl name clauses
    env <- getEnv
    let sch = generalize env ps ty
    return (s, ps, singletonEnv name sch)
  DeclTypeSig name sch ->
    return (emptySubst, [], singletonEnv name sch)
  _ -> throwError $ OtherError $ "inferValueDecl: unsupported declaration: " ++ show decl
inferDecls :: [Decl] -> InferM (Subst, [Pred], TypeEnv)
inferDecls decls = do
  results <- mapM inferValueDecl decls
  let (substs, predsList, envs) = unzip3 results
      s = composeMany substs
      ps = concat predsList
      env = foldr mergeEnvs emptyEnv envs
  return (s, ps, env)
-}
