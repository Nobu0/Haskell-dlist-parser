module Language.TypeSystem.PatternInfer where

-- import AST.Pattern
import Control.Monad
-- (TypeLike (..))

import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.BaseType
import Language.TypeSystem.Class
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Env
import Language.TypeSystem.Error
import Language.TypeSystem.InferInstances
import Language.TypeSystem.InferM
import Language.TypeSystem.Pattern
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax
import Language.TypeSystem.Unify


inferPattern :: Pattern -> InferM (Subst, TypeEnv, Type)
inferPattern pat = case pat of
  PVar name -> do
    tv <- freshTypeVar
    return (emptySubst, TypeEnv (Map.singleton name (Forall [] [] tv)), tv)
  PWildcard -> do
    tv <- freshTypeVar
    return (emptySubst, TypeEnv Map.empty, tv)
  PInt _ -> return (emptySubst, TypeEnv Map.empty, TCon "Int")
  PChar _ -> return (emptySubst, TypeEnv Map.empty, TCon "Char")
  PString _ -> return (emptySubst, TypeEnv Map.empty, TApp (TCon "List") (TCon "Char"))
  PCons p1 p2 -> do
    (s1, env1, t1) <- inferPattern p1
    (s2, env2, t2) <- inferPattern p2
    s3 <- unify (applySubst s2 t2) (TList (applySubst s2 t1))
    let s = composeMany [s3, s2, s1]
    return (s, combineEnvs [env1, env2], applySubst s t2)
  PList ps -> do
    results <- mapM inferPattern ps
    let (subs, envs, types) = unzip3 results
    tv <- freshTypeVar
    mapM_ (\t -> unify t tv) types
    let s = composeMany subs
    return (s, combineEnvs (map (applySubst s) envs), TList tv)
  PTuple ps -> do
    results <- mapM inferPattern ps
    let (subs, envs, types) = unzip3 results
    let s = composeMany subs
    return (s, combineEnvs (map (applySubst s) envs), TTuple types)
  PAs name p -> do
    (s1, env1, t1) <- inferPattern p
    -- let env2 = Map.insert name (Forall [] [] t1) env1
    let TypeEnv envMap = env1
        env2 = TypeEnv (Map.insert name (Forall [] [] t1) envMap)
    return (s1, env2, t1)
  PConstr name args -> inferConstrPattern name args
  PApp f args -> inferPattern (PConstr (extractConstrName f) args)
  PInfix p1 name p2 -> inferPattern (PConstr name [p1, p2])

inferConstrPattern :: Name -> [Pattern] -> InferM (Subst, TypeEnv, Type)
inferConstrPattern name args = do
  mScheme <- lookupConstructor name
  case mScheme of
    Nothing -> throwError $ UnboundVariable name
    Just scheme -> do
      t <- instantiate scheme
      inferConstrApp t args

-- | 構築子型に引数パターンを適用して型を推論
inferConstrApp :: Type -> [Pattern] -> InferM (Subst, TypeEnv, Type)
inferConstrApp t [] = return (emptySubst, TypeEnv Map.empty, t)
inferConstrApp t (p : ps) = do
  tv <- freshTypeVar
  let tExpected = TArrow tv (TVar "res")
  s1 <- unify t tExpected
  let TArrow tArg tRest = applySubst s1 t
  (s2, env1, t1) <- inferPattern p
  s3 <- unify (applySubst s2 tv) t1
  (s4, env2, tFinal) <- inferConstrApp (applySubst s3 tRest) ps
  let s = composeMany [s4, s3, s2, s1]
  return (s, combineEnvs [env1, env2], applySubst s tFinal)

extractConstrName :: Pattern -> Name
extractConstrName (PVar n) = n
extractConstrName (PConstr n []) = n
extractConstrName _ = error "PApp must start with a constructor or variable"

-- この関数は型環境やデータ型情報に基づいて構築子の型を返す必要がある
-- 今は仮に未実装としておく
-- lookupConstructor :: Name -> InferM (Maybe Scheme)
-- lookupConstructor name = throwError $ OtherError $ "lookupConstructor not implemented: " ++ name
