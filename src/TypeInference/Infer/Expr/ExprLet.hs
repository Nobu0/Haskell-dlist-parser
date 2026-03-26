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
import Data.List (partition)
import qualified Data.Map as M
-- import qualified TypeEnv (fromList)
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

{-}
inferLet inferExprFn env pat e1 e2 = do
  (sPat, envPat, tPat) <- inferPattern pat
  (s1, t1) <- inferExprFn (applyEnv sPat env) e1
  s2 <- lift $ first InferUnifyError (unify (apply s1 tPat) t1)
  let s = s2 `composeSubst` s1 `composeSubst` sPat
  let env' = mergeEnvs (applyEnv s env) (applyEnv s envPat)
  (s3, t2) <- inferExprFn env' e2
  let sFinal = s3 `composeSubst` s

  return (sFinal, t2)
-}
{-}
inferBindings inferExprFn env [] = return (emptySubst, emptyEnv)
inferBindings inferExprFn env ((pat, expr) : rest) = do
  myTraceE ("<< inferBindings: pat " ++ show pat)
  (sPat, envPat, tPat) <- inferPattern pat
  (sExpr, tExpr) <- inferExprFn (applyEnv sPat env) expr
  sUnify <- lift $ first InferUnifyError (unify (apply sExpr tPat) tExpr)
  let s = sUnify `composeSubst` sExpr `composeSubst` sPat
  let env' = applyEnv s envPat
  (sRest, envRest) <- inferBindings inferExprFn (applyEnv s env) rest
  let sFinal = sRest `composeSubst` s
  let envFinal = mergeEnvs env' envRest
  return (sFinal, envFinal)
-}

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

{-}
buildEnvFrom :: TypeEnv -> [(Pattern, Type)] -> TypeEnv
buildEnvFrom env pts =
  let entries = [(nameFromPat pat, generalize env ty) | (pat, ty) <- pts]
   in extendEnvs emptyEnv entries
-}
-- buildEnvFrom :: TypeEnv -> [(Pattern, Scheme)] -> TypeEnv
-- buildEnvFrom _ = fromList . map (\(PVar name, scheme) -> (name, scheme))
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

{-}
inferWhere ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [(Pattern, Expr)] ->
  InferM (Subst, Type)
inferWhere inferExprFn env expr binds = do
  let (funBinds, valBinds) = partition isFunBind binds

  -- 関数定義を先に処理
  funResults <- mapM (inferFunBinding inferExprFn env) funBinds
  let (sFunList, funTypesList) = unzip funResults
      sFun = foldr composeSubst emptySubst sFunList
      funTypes = concat funTypesList
      envFun = buildEnvFrom funTypes

  -- 値定義を処理
  (sVal, envVal, assumptions) <- inferPatterns (map fst valBinds)
  bindResults <- mapM (inferBinding inferExprFn (mergeEnvs envFun (applyEnv sVal envVal))) valBinds
  let (s2s, bindTypesList) = unzip bindResults
      s2 = foldr composeSubst emptySubst s2s
      bindTypes = concat bindTypesList
  s3 <- unifyBindings assumptions bindTypes
  let envFinal = applyEnv s3 (applyEnv s2 env'') -- 環境に全ての代入を適用
  (s4, tBody) <- inferExprFn envFinal expr
  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  return (s, apply s tBody)
-}

{-}
--inferWhere inferExprFn env expr binds = do
--  (s1, envFromPats, assumptions) <- inferPatterns (map fst binds)
--  let env'' = mergeEnvs env (applyEnv s1 envFromPats)
--  bindResults <- mapM (inferBinding inferExprFn env'') binds

inferWhere inferExprFn env expr binds = do
  -- 1. すべてのバインディングに型変数を割り当てる
  (s1, env', assumptions) <- inferPatterns (map fst binds)

  -- 2. 拡張環境で右辺を推論
  -- (s2, bindTypes) <- inferBinding inferExprFn (applyEnv s1 env') binds
  bindResults <- mapM (inferBinding inferExprFn (applyEnv s1 env')) binds

  -- 3. unify して型を確定
  s3 <- unifyBindings assumptions bindTypes

  -- 4. 本体を推論
  (s4, tBody) <- inferExprFn (applyEnv s3 (applyEnv s2 (applyEnv s1 env))) expr

  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  return (s, apply s tBody)

inferWhere inferExprFn env e binds = do
  myTraceE $ "<< inferWhere:* binds count = " ++ show (length binds)

  -- 1. binds を先に処理して環境に追加！
  (s1, env') <-
    foldM
      ( \(sAcc, envAcc) (pat, expr) -> do
          -- myTraceE $ "<< inferBinding: pat = " ++ show pat
          (sNew, envNext) <- inferBinding inferExprFn envAcc (pat, expr)
          let sCombined = sNew `composeSubst` sAcc
          return (sCombined, envNext)
      )
      (emptySubst, env)
      binds

  -- 2. 拡張された環境で本体を推論！
  (s2, t2) <- inferExprFn (applyEnv s1 env') e

  return (s2 `composeSubst` s1, t2)
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

{-}
inferBinding ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  (Pattern, Expr) ->
  InferM (Subst, [(Pattern, Type)])
--  InferM (Subst, TypeEnv)

inferBinding inferExprFn env (pat, expr) = do
  myTraceE $ "<< inferBinding: pat = " ++ show pat

  -- 1. パターンから型と環境を推論
  (sPat, envFromPat, patType) <- inferPattern pat

  -- 2. 式を推論
  (sExpr, exprType) <- inferExprFn (applyEnv sPat envFromPat) expr

  -- 3. パターンの型と式の型を unify
  -- sUnify <- unify (apply sExpr patType) exprType
  sUnify <- liftEither (mapLeft InferUnifyError $ unify (apply sExpr patType) exprType)

  -- 4. 環境を構築
  let sAll = sUnify `composeSubst` sExpr `composeSubst` sPat
  let finalEnv = applyEnv sAll envFromPat
  let name = nameFromPat pat
  let scheme = generalize (applyEnv sAll env) (apply sAll exprType)
  let env' = extendEnv env name scheme

  -- myTraceE $ ">> binding name = " ++ name ++ ", scheme = " ++ show scheme
  -- return (sAll, env')
  return (sAll, assumptions)
-}

{-}
inferBinding inferExprFn env (pat, expr) = do
  myTraceE $ "<< inferBinding: pat = " ++ show pat ++ " env " ++ show env

  -- 1. パターンから型と環境を推論
  (sPat, envFromPat, patType) <- inferPattern pat

  -- 2. 式を推論
  let env' = mergeEnvs env (applyEnv sPat envFromPat)
  (sExpr, exprType) <- inferExprFn env' expr
  -- (sExpr, exprType) <- inferExprFn (applyEnv sPat envFromPat) expr

  -- 3. パターンの型と式の型を unify
  sUnify <- liftEither (mapLeft InferUnifyError $ unify (apply sExpr patType) exprType)

  -- 4. 代入を合成
  let sAll = sUnify `composeSubst` sExpr `composeSubst` sPat

  -- 5. [(Pattern, Type)] を返す
  return (sAll, [(pat, apply sAll patType)])
-}

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left e) = Left (f e)
mapLeft _ (Right x) = Right x

{-}
nameFromPat :: Pattern -> Name
nameFromPat (PVar name) = name
nameFromPat (PApp (PVar name) _) = name
nameFromPat (PApp (PConstr name _) _) = name
nameFromPat (PApp pat _) = nameFromPat pat
nameFromPat (PCons pat1 _) = nameFromPat pat1
nameFromPat pat = error $ "nameFromPat: unsupported pattern " ++ show pat
-}

nameFromPat :: Pattern -> Name
nameFromPat (PVar name) = name
nameFromPat (PApp (PVar name) _) = name
nameFromPat (PApp pat _) = nameFromPat pat
nameFromPat (PCons pat1 _) = nameFromPat pat1
nameFromPat pat = error $ "nameFromPat: unsupported pattern " ++ show pat

{-}
nameFromPat :: Pattern -> Name
nameFromPat (PVar name) = name
nameFromPat (PApp (PVar name) _) = name
nameFromPat pat = error $ "nameFromPat: unsupported pattern " ++ show pat
-}
