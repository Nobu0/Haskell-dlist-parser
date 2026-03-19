module TypeInference.Infer.Pattern
  ( inferPattern,
    inferPatterns,
    inferPatternApp,
  )
where

import AST.Decl (Decl (..))
-- import TypeInference.TypeEnv
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
import AST.Pattern (Pattern (..))
import AST.Type (Type (..))
import qualified Control.Exception as TypeInference
import Control.Monad (foldM)
-- import TypeInference.SQLInfer

import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Error (InferError (..))
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (UnifyError (..), unify)
import Utils.MyTrace

inferPattern :: Pattern -> InferM (Subst, TypeEnv, Type)
inferPattern pat = do
  myTraceE ("<< infrePattern: pat " ++ show pat)
  case pat of
    -- 変数パターン
    PVar x -> do
      tv <- freshTypeVar
      let env = extendEnv emptyEnv x (Forall [] tv)
      return (emptySubst, env, tv)

    -- 単一変数の PApp パターン
    PApp (PVar x) [] -> do
      tv <- freshTypeVar
      let env = extendEnv emptyEnv x (Forall [] tv)
      return (emptySubst, env, tv)

    -- PApp の一般形（引数なし）
    PApp p [] -> inferPattern p
    -- 整数リテラル
    PInt _ ->
      return (emptySubst, emptyEnv, TCon "Int")
    -- ワイルドカード
    PWildcard -> do
      let t = TVar "t_wild"
      return (emptySubst, emptyEnv, t)

    -- リストパターン [a, b, c]
    PList ps -> do
      (s, env, ts) <- inferPatterns ps
      case ts of
        [] -> do
          tv <- freshTypeVar
          return (s, env, TList tv)
        (t0 : _) -> do
          s' <-
            foldM
              ( \sacc t ->
                  case unify (apply sacc t) (apply sacc t0) of
                    Left _ -> lift $ Left (InferMismatch (apply sacc t) (apply sacc t0))
                    Right s -> return (composeSubst s sacc)
              )
              s
              ts
          let tElem = apply s' t0
          return (s', env, TList tElem)

    -- タプルパターン (a, b, c)
    PTuple ps -> do
      (s, env, ts) <- inferPatterns ps
      return (s, env, TTuple ts)

    -- コンストラクタパターン Just x, Pair a b
    PConstr con args -> do
      case lookupEnv builtinPatternEnv con of
        Nothing -> lift $ Left (InferOther ("Unknown constructor: " ++ con))
        Just scheme -> do
          tCon <- lift $ instantiate scheme
          inferPatternApp tCon args

    -- Cons パターン (x:xs)
    PCons p1 p2 -> do
      (s1, env1, t1) <- inferPattern p1
      (s2, env2, t2) <- inferPattern p2
      s3 <- case unify (apply s2 t2) (TList t1) of
        Left _ -> lift $ Left (InferMismatch t2 (TList t1))
        Right s -> return s
      let s = composeSubst s3 (composeSubst s2 s1)
          env = mergeEnvs env1 env2
      return (s, env, apply s (TList t1))

    -- As パターン x@p
    PAs name p -> do
      (s1, env1, t1) <- inferPattern p
      let env2 = extendEnv env1 name (Forall [] t1)
      return (s1, env2, t1)

inferPatternApp :: Type -> [Pattern] -> InferM (Subst, TypeEnv, Type)
inferPatternApp tCon [] =
  return (emptySubst, emptyEnv, tCon)
inferPatternApp tCon (p : ps) = do
  (s1, env1, tArg) <- inferPattern p
  let alpha = TVar "t_app"
  s2 <- case unify (apply s1 tCon) (TArrow tArg alpha) of
    Left _ -> lift $ Left (InferMismatch (apply s1 tCon) (TArrow tArg alpha))
    Right s -> return s
  (s3, env2, tRes) <- inferPatternApp (apply s2 alpha) ps
  let s = composeSubst s3 (composeSubst s2 s1)
      env = mergeEnvs env1 env2
  return (s, env, apply s tRes)

{-}
inferPattern :: Pattern -> InferM (Subst, TypeEnv, Type)
inferPattern pat =
  case pat of
    PVar x -> do
      tv <- freshTypeVar
      let env = extendEnv emptyEnv x (Forall [] tv)
      return (emptySubst, env, tv)

    PApp (PVar x) [] -> do
      tv <- freshTypeVar
      let env = extendEnv emptyEnv x (Forall [] tv)
      return (emptySubst, env, tv)

    PApp p [] -> inferPattern p

    PInt _ ->
      return (emptySubst, emptyEnv, TCon "Int")

    PWildcard -> do
      let t = TVar "t_wild"
      return (emptySubst, emptyEnv, t)

    PList ps -> do
      (s, env, ts) <- inferPatterns ps
      case ts of
        [] -> do
          tv <- freshTypeVar
          return (s, env, TList tv)
        (t0 : _) -> do
          s' <-
            foldM
              (\sacc t ->
                 lift $ case unify (apply sacc t) (apply sacc t0) of
                   Left _ -> Left (InferMismatch (apply sacc t) (apply sacc t0))
                   Right s -> Right s
              )
              s
              ts
          let tElem = apply s' t0
          return (s', env, TList tElem)

    PTuple ps -> do
      (s, env, ts) <- inferPatterns ps
      return (s, env, TTuple ts)

    PConstr con args -> do
      case lookupEnv builtinPatternEnv con of
        Nothing -> lift $ Left (InferOther ("Unknown constructor: " ++ con))
        Just scheme -> do
          tCon <- lift $ instantiate scheme
          inferPatternApp tCon args

    PCons p1 p2 -> do
      (s1, env1, t1) <- inferPattern p1
      (s2, env2, t2) <- inferPattern p2
      s3 <- lift $ case unify (apply s2 t2) (TList t1) of
        Left _ -> Left (InferMismatch t2 (TList t1))
        Right s -> Right s
      let s = composeSubst s3 (composeSubst s2 s1)
          env = mergeEnvs env1 env2
      return (s, env, apply s (TList t1))

    PAs name p -> do
      (s1, env1, t1) <- inferPattern p
      let env2 = extendEnv env1 name (Forall [] t1)
      return (s1, env2, t1)

inferPatternApp :: Type -> [Pattern] -> InferM (Subst, TypeEnv, Type)
inferPatternApp tCon [] =
  return (emptySubst, emptyEnv, tCon)
inferPatternApp tCon (p : ps) = do
  (s1, env1, tArg) <- inferPattern p
  let alpha = TVar "t_app"
  s2 <- lift $ case unify (apply s1 tCon) (TArrow tArg alpha) of
    Left _ -> Left (InferMismatch (apply s1 tCon) (TArrow tArg alpha))
    Right s -> Right s
  (s3, env2, tRes) <- inferPatternApp (apply s2 alpha) ps
  let s = composeSubst s3 (composeSubst s2 s1)
      env = mergeEnvs env1 env2
  return (s, env, apply s tRes)
-}

inferPatterns :: [Pattern] -> InferM (Subst, TypeEnv, [Type])
inferPatterns [] = return (emptySubst, emptyEnv, [])
inferPatterns (p : ps) = do
  (s1, env1, t1) <- inferPattern p
  (s2, env2, ts) <- inferPatterns ps
  let s = composeSubst s2 s1
  let env = mergeEnvs env1 env2
  return (s, env, t1 : ts)
