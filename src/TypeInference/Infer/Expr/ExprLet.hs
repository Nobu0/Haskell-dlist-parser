module TypeInference.Infer.Expr.ExprLet
  ( inferLet,
    inferLetBlock,
    inferWhere,
    inferBindings,
    inferBinding,
  )
where

import AST.Expr (Expr (..), Name)
import AST.Pattern
-- import AST.Type
import Control.Monad (foldM)
-- import qualified TypeInference.Type as TI

-- import TypeInference.Types

import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bifunctor (first)
-- import qualified TypeEnv (fromList)

import Data.List (nub, partition)
import qualified Data.Map as M
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprFun
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.Type (Type (..))
import TypeInference.TypeEnv
import TypeInference.Unify (unify)
import Utils.MyTrace (myTraceE)

inferLet ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Pattern ->
  Expr ->
  Expr ->
  InferM (Subst, Type)
inferLet inferExpr env pat expr body
  | isFunBind (pat, expr) = do
      (sFun, funTypes) <- inferFunBinding inferExpr env (pat, expr)
      let envFun = buildEnvFrom env (generalizeAll env funTypes)
          env' = mergeEnvs (applyEnv sFun envFun) env
      -- myTraceE ("<< inferLet: envFun " ++ show envFun)
      -- myTraceE ("<< inferLet: env' " ++ show env')
      (sBody, tBody) <- inferExpr env' body
      return (sBody `composeSubst` sFun, tBody)
  | otherwise = do
      (s1, env1, patType) <- inferPattern pat
      let env1' = applyEnv s1 env1
          env' = mergeEnvs env1' env
      (s2, exprType) <- inferExpr env' expr
      s3 <- liftEither (mapLeft InferUnifyError $ unify (apply s2 patType) exprType)
      let s = s3 `composeSubst` s2 `composeSubst` s1
      (sBody, tBody) <- inferExpr (applyEnv s env') body
      return (sBody `composeSubst` s, apply s tBody)

inferBindings ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [(Pattern, Expr)] ->
  InferM (Subst, TypeEnv)
inferBindings inferExpFn env [] = return (emptySubst, emptyEnv)
inferBindings inferExprFn env ((pat, expr) : bs) = do
  myTraceE ("<< inferBindings: pat " ++ show pat ++ " expr " ++ show expr)
  -- 右辺の式の型を推論
  (s1, t1) <- inferExprFn env expr
  -- パターンの型を推論
  (s2, env1, tPat) <- inferPattern pat
  -- 型を unify する
  s3 <- unifyM (apply s2 tPat) (apply s2 t1)
  -- 環境と置換を更新
  let s = s3 `composeSubst` s2 `composeSubst` s1
      env' = applyEnv s env
      env1' = applyEnv s env1
  -- 残りのバインディングを処理
  (sRest, envRest) <- inferBindings inferExprFn env' bs
  return (sRest `composeSubst` s, env1' `mergeEnvs` envRest)

inferLetBlock ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [(Pattern, Expr)] ->
  Expr ->
  InferM (Subst, Type)
inferLetBlock inferExprFn env binds body = do
  (sBinds, envBinds) <- inferBindings inferExprFn env binds
  let env' = mergeEnvs envBinds env
  let env'' = applyEnv sBinds env'
  inferExprFn env'' body

isFunBind :: (Pattern, Expr) -> Bool
isFunBind (PVar _, ELam _ _) = True
isFunBind (PApp (PVar _) _, _) = True
isFunBind _ = False

buildEnvFrom :: TypeEnv -> [(Pattern, Scheme)] -> TypeEnv
buildEnvFrom _ binds =
  fromList [(name, scheme) | (pat, scheme) <- binds, name <- patNames pat]

patNames :: Pattern -> [String]
patNames (PVar name) = [name]
patNames (PApp f args) = patNames f ++ concatMap patNames args
patNames _ = []

extendEnvs :: TypeEnv -> [(Name, Scheme)] -> TypeEnv
extendEnvs = foldl (\env (name, scheme) -> extendEnv env name scheme)

inferWhere ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [(Pattern, Expr)] ->
  InferM (Subst, Type)
inferWhere inferExprFn env expr binds = do
  myTraceE ("<< inferWhere: binds " ++ show binds)
  let (funBinds, valBinds) = partition isFunBind binds
  myTraceE ("<< inferWhere: funBinds " ++ show funBinds)

  -- 🔧 1. 関数名を収集して仮の型変数を割り当てる
  let funNames = nub [name | (PApp (PVar name) _, _) <- funBinds]
  tvs <- mapM (const freshTypeVar) funNames
  let tempFunEnv = TypeEnv (M.fromList (zip funNames (map (Forall []) tvs)))
      envWithTempFuns = mergeEnvs tempFunEnv env

  -- 🔧 2. 関数定義を推論（仮環境を使う）
  funResults <- mapM (inferFunBinding inferExprFn envWithTempFuns) funBinds
  myTraceE ("<< inferWhere: funResults " ++ show funResults)
  let (sFunList, funTypesList) = unzip funResults
      sFun = foldr composeSubst emptySubst sFunList
      funTypes = concat funTypesList
      envFun = buildEnvFrom env (generalizeAll env funTypes)
      env'' = mergeEnvs (applyEnv sFun envFun) env

  -- 値定義を処理
  myTraceE ("<< inferWhere: env'' " ++ show env'')
  myTraceE ("<< inferFunClause: funTypes = " ++ show funTypes)
  (sVal, envVal, assumptions) <- inferPatterns (map fst valBinds)
  let envVal' = applyEnv sVal envVal
  let envForVals = mergeEnvs envVal' env''
  myTraceE ("<< inferWhere: envForVals " ++ show envForVals)
  bindResults <- mapM (inferBinding inferExprFn envForVals) valBinds
  let (s2s, envs, bindTypesList) = unzip3 bindResults
      envVal = foldr mergeEnvs emptyEnv envs
      s2 = foldr composeSubst emptySubst s2s
      bindTypes = concat bindTypesList

  s3 <- unifyBindings assumptions bindTypes
  let envFinal = applyEnv s3 (applyEnv s2 (mergeEnvs env'' envVal))
  (s4, tBody) <- inferExprFn envFinal expr
  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` sVal `composeSubst` sFun
  return (s, apply s tBody)

{-}
inferWhere inferExprFn env expr binds = do
  myTraceE ("<< inferWhere: binds " ++ show binds)
  let (funBinds, valBinds) = partition isFunBind binds
  myTraceE ("<< inferWhere: funBinds " ++ show funBinds)

  -- 関数定義を先に処理
  funResults <- mapM (inferFunBinding inferExprFn env) funBinds
  myTraceE ("<< inferWhere: funResults " ++ show funResults)
  let (sFunList, funTypesList) = unzip funResults
      sFun = foldr composeSubst emptySubst sFunList
      funTypes = concat funTypesList
      -- envFun = buildEnvFrom env funTypes
      envFun = buildEnvFrom env (generalizeAll env funTypes)
      env'' = mergeEnvs (applyEnv sFun envFun) env

  -- 値定義を処理
  myTraceE ("<< inferWhere: env'' " ++ show env'')
  myTraceE ("<< inferFunClause: funTypes = " ++ show funTypes)
  (sVal, envVal, assumptions) <- inferPatterns (map fst valBinds)
  let envVal' = applyEnv sVal envVal
  let envForVals = mergeEnvs envVal' env''
  myTraceE ("<< inferWhere: envForVals " ++ show envForVals)
  bindResults <- mapM (inferBinding inferExprFn envForVals) valBinds
  let (s2s, envs, bindTypesList) = unzip3 bindResults
      envVal = foldr mergeEnvs emptyEnv envs
      s2 = foldr composeSubst emptySubst s2s
      bindTypes = concat bindTypesList

  s3 <- unifyBindings assumptions bindTypes
  let envFinal = applyEnv s3 (applyEnv s2 (mergeEnvs env'' envVal))
  (s4, tBody) <- inferExprFn envFinal expr
  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` sVal `composeSubst` sFun
  return (s, apply s tBody)
-}

inferBinding ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  (Pattern, Expr) ->
  InferM (Subst, TypeEnv, [(Pattern, Type)])
inferBinding inferExprFn env bind@(pat, expr)
  | isFunBind bind = do
      (s, funTypes) <- inferFunBinding inferExprFn env bind
      let env' = buildEnvFrom env (generalizeAll env funTypes)
      return (s, env', funTypes)
  | otherwise = do
      myTraceE $ "<< inferBinding: pat = " ++ show pat ++ " env " ++ show env
      (sPat, envFromPat, patType) <- inferPattern pat
      let env' = mergeEnvs (applyEnv sPat envFromPat) env
      (sExpr, exprType) <- inferExprFn env' expr
      sUnify <- liftEither (mapLeft InferUnifyError $ unify (apply sExpr patType) exprType)
      let sAll = sUnify `composeSubst` sExpr `composeSubst` sPat
      return (sAll, envFromPat, [(pat, apply sAll patType)])

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left e) = Left (f e)
mapLeft _ (Right x) = Right x

nameFromPat :: Pattern -> Name
nameFromPat (PVar name) = name
nameFromPat (PApp (PVar name) _) = name
nameFromPat (PApp pat _) = nameFromPat pat
nameFromPat (PCons pat1 _) = nameFromPat pat1
nameFromPat pat = error $ "nameFromPat: unsupported pattern " ++ show pat
