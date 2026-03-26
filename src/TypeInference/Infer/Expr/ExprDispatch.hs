module TypeInference.Infer.Expr.ExprDispatch (inferExpr) where

import AST.Expr
-- import AST.Type
-- import AST.Type (Type (..)) -- これで TFun などのコンストラクタが使えるようになる

-- import Data.Map

-- import TypeInference.Type (Type (..))

-- import qualified TypeInference.Type as TI

-- import qualified TypeInference.Unify as Unify

import AST.Pattern (Pattern)
import qualified AST.Type as AST
import Control.Monad.State (get, put)
import Data.Bifunctor (first)
import qualified Data.Map as M
import qualified Data.Map as Map
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprApp (inferApp)
import TypeInference.Infer.Expr.ExprBinOp (inferBinOp, inferOpSectionL, inferOpSectionR)
import TypeInference.Infer.Expr.ExprCase (inferCase)
import TypeInference.Infer.Expr.ExprDo (inferDo)
import TypeInference.Infer.Expr.ExprIf (inferIf)
import TypeInference.Infer.Expr.ExprLet (inferBindings, inferLet, inferLetBlock, inferWhere)
import TypeInference.Infer.Expr.ExprLiteral (inferBool, inferChar, inferInt, inferList, inferString, inferTuple)
import TypeInference.Infer.Expr.ExprSQL (inferSQL)
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.Type
import TypeInference.TypeEnv
import Utils.MyTrace

-- 他の構文モジュールもここに import
inferExpr :: TypeEnv -> Expr -> InferM (Subst, Type)
inferExpr _ (EVar "__unit__") = return (emptySubst, TTuple [])
inferExpr env (ESeq []) =
  lift $ Left (InferOther "Empty expression sequence")
inferExpr env (ESeq [e]) = do
  -- myTraceE ("<< inferExpr: env " ++ show env)
  inferExpr env e
inferExpr env (ESeq (e : es)) = do
  myTraceE ("<< inferExpr: e " ++ show e)
  (s1, _) <- inferExpr env e
  let env' = applyEnv s1 env
  (s2, t2) <- inferExpr env' (ESeq es)
  return (s2 `composeSubst` s1, t2)
{-}
inferExpr env (EVar name) = do
  myTraceE ("<< inferExpr: name " ++ show name)
  case lookupEnv env name of
    Nothing -> lift $ Left (InferUnboundVariable name)
    Just sigma -> do
      t <- lift $ instantiate sigma
      return (emptySubst, t)
-}
inferExpr env (EVar name) = do
  myTraceE ("<< inferExpr: name " ++ show name)
  -- myTraceE ("<< inferExpr: name " ++ show name ++ " env " ++ show env)
  case lookupEnv env name of
    Just scheme -> do
      t <- instantiate2 scheme
      return (emptySubst, t)
    Nothing ->
      lift $ Left (InferUnboundVariable name)

-- AST で定義された型で分岐
inferExpr env expr = case expr of
  ELet pat e1 e2 -> inferLet inferExpr env pat e1 e2
  ELetBlock binds body -> inferLetBlock inferExpr env binds body
  EWhere e binds -> inferWhere inferExpr env e binds
  EIf c t f -> inferIf inferExpr env c t f
  EDo stmts -> inferDo inferExpr env stmts
  ECase scrut alts -> inferCase inferExpr env scrut alts
  EApp e1 e2 -> inferApp inferExpr env e1 e2
  EBinOp op e1 e2 -> inferBinOp inferExpr env op e1 e2
  EInt _ -> inferInt
  EBool _ -> inferBool
  EString _ -> inferString
  EChar _ -> inferChar
  ETuple es -> inferTuple inferExpr env es
  EList es -> inferList inferExpr env es
  EUnit -> inferUnit
  ESQL _ params -> inferSQL inferExpr env params
  EOpSectionL op e -> inferOpSectionL inferExpr env op e
  EOpSectionR e op -> inferOpSectionR inferExpr env e op
  ERecord fields -> inferRecord inferExpr env fields
  EFieldAccess e fields -> inferField inferExpr env e fields
  EReturn e -> inferReturn inferExpr env e
  ELam pat e -> inferELam inferExpr env pat e
  EListComp e qa -> inferListComp inferExpr env e qa
  ERange e1 e2 -> inferRange inferExpr env e1 e2
  ERangeStep e1 e2 e3 -> inferRangeStep inferExpr env e1 e2 e3

instantiate2 :: Scheme -> InferM Type
instantiate2 (Forall vars t) = do
  nvars <- mapM (const fresh) vars
  let s = M.fromList (zip vars nvars)
  return $ apply s t

fresh :: InferM Type
fresh = do
  n <- get
  put (n + 1)
  return $ TVar ("t" ++ show n)

inferRangeStep ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  Expr ->
  Maybe Expr ->
  InferM (Subst, Type)
inferRangeStep inferExprFn env start step maybeEnd = do
  (s1, t1) <- inferExprFn env start
  (s2, t2) <- inferExprFn (applyEnv s1 env) step
  s3 <- unifyM (apply s2 t1) t2
  case maybeEnd of
    Just end -> do
      (s4, t3) <- inferExprFn (applyEnv s3 (applyEnv s2 (applyEnv s1 env))) end
      s5 <- unifyM (apply s4 t1) t3
      let s = s5 `composeSubst` s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
      return (s, TList (apply s t1))
    Nothing -> do
      let s = s3 `composeSubst` s2 `composeSubst` s1
      return (s, TList (apply s t1))

inferRange ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  Maybe Expr ->
  InferM (Subst, Type)
inferRange inferExpr env start end =
  case end of
    Just e -> do
      (s1, t1) <- inferExpr env start
      (s2, t2) <- inferExpr env e
      s3 <- unifyM t1 t2
      return (s3 `composeSubst` s2 `composeSubst` s1, TList (apply s3 t1))
    Nothing -> do
      (s1, t1) <- inferExpr env start
      return (s1, TList t1)

inferListComp ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [Qualifier] ->
  InferM (Subst, Type)
inferListComp inferExprFn env expr stmts = do
  (s1, env') <- inferQualifiers inferExprFn env stmts
  (s2, t) <- inferExpr (applyEnv s1 env') expr
  return (s2 `composeSubst` s1, TList t)

inferQualifiers ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [Qualifier] ->
  InferM (Subst, TypeEnv)
inferQualifiers inferExprFn env [] = return (emptySubst, env)
inferQualifiers inferExprFn env (q : qs) = case q of
  QGenerator pat genExpr -> do
    (s1, tGen) <- inferExpr env genExpr
    tv <- freshTypeVar
    s2 <- unifyM tGen (TList tv)
    (s3, env1, tPat) <- inferPattern pat
    s4 <- unifyM (apply s3 tv) (apply s3 tPat)
    let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
        env' = applyEnv s (env `mergeEnvs` env1)
    (sRest, envRest) <- inferQualifiers inferExprFn env' qs
    return (sRest `composeSubst` s, envRest)
  QLet binds -> do
    (s1, env1) <- inferBindings inferExprFn env binds
    let env' = applyEnv s1 (env `mergeEnvs` env1)
    (sRest, envRest) <- inferQualifiers inferExprFn env' qs
    return (sRest `composeSubst` s1, envRest)
  QGuard condExpr -> do
    (s1, tCond) <- inferExpr env condExpr
    s2 <- unifyM tCond (TCon "Bool")
    (sRest, envRest) <- inferQualifiers inferExprFn (applyEnv s2 env) qs
    return (sRest `composeSubst` s2 `composeSubst` s1, envRest)

inferELam ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Pattern ->
  Expr ->
  InferM (Subst, Type)
inferELam inferExprFn env pat body = do
  myTraceE ("<< inferELam: pat " ++ show pat)
  -- パターンの型と環境を推論
  (s1, env1, t1) <- inferPattern pat

  -- パターンで拡張された環境で本体を推論
  (s2, t2) <- inferExprFn (applyEnv s1 (env `mergeEnvs` env1)) body

  let s = s2 `composeSubst` s1
  return (s, TArrow (apply s t1) t2)

inferReturn ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  InferM (Subst, Type)
inferReturn inferExprFn env e = do
  myTraceE ("<< inferReturn: e " ++ show e)
  (s, t) <- inferExprFn env e
  return (s, TApp (TCon "IO") t)

inferUnit :: InferM (Subst, Type)
inferUnit = return (emptySubst, TCon "Unit")

inferField ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  String ->
  InferM (Subst, Type)
inferField infer env e field = do
  (s1, t1) <- inferExpr env e
  tv <- freshTypeVar
  s2 <- unifyM t1 (TRecord (Map.singleton field tv))
  return (s2 `composeSubst` s1, apply s2 tv)

inferRecord ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [(String, Expr)] ->
  InferM (Subst, Type)
inferRecord infer env fields = do
  inferred <-
    mapM
      ( \(name, expr) -> do
          (s, t) <- infer env expr
          return (s, (name, t))
      )
      fields
  let substs = map fst inferred
      fieldTypes = map snd inferred
      s = foldr composeSubst emptySubst substs
      typedFields = map (\(name, t) -> (name, apply s t)) fieldTypes
  return (s, TRecord (Map.fromList typedFields))
