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
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Error (InferError (..))
import TypeInference.Infer.Core
-- import TypeInference.SQLInfer
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (UnifyError (..), unify)

inferPattern :: Pattern -> Either InferError (Subst, TypeEnv, Type)
inferPattern pat = case pat of
  -- 変数パターン
  PVar x -> do
    tv <- freshTypeVar
    let env = extendEnv emptyEnv x (Forall [] tv)
    Right (emptySubst, env, tv)
  -- 単一変数の PApp パターン（あなたのパーサーが生成する形）
  PApp (PVar x) [] -> do
    tv <- freshTypeVar
    let env = extendEnv emptyEnv x (Forall [] tv)
    Right (emptySubst, env, tv)
  -- ★ 追加：PApp の一般形（引数なし）
  PApp p [] -> inferPattern p
  {-}
  -- 変数パターン
  PVar v ->
    let t = TVar ("t_" ++ v)
        env = extendEnv emptyEnv v (Forall [] t)
     in Right (emptySubst, env, t)
     -}
  -- 整数リテラル
  PInt _ ->
    Right (emptySubst, emptyEnv, TCon "Int")
  -- ワイルドカード
  PWildcard ->
    let t = TVar "t_wild"
     in Right (emptySubst, emptyEnv, t)
  -- リストパターン [a, b, c]
  PList ps -> do
    (s, env, ts) <- inferPatterns ps
    case ts of
      [] -> Right (s, env, TList (TVar "t_empty"))
      (t0 : _) -> do
        -- 全要素の型を t0 と unify
        -- s' <- foldM (\sacc t -> unify (apply sacc t) (apply sacc t0)) s ts
        s' <-
          foldM
            ( \sacc t ->
                case unify (apply sacc t) (apply sacc t0) of
                  Left _ ->
                    Left (InferMismatch (apply sacc t) (apply sacc t0))
                  Right s ->
                    Right s
            )
            s
            ts
        let tElem = apply s' t0
        Right (s', env, TList tElem)

  -- タプルパターン (a, b, c)
  PTuple ps -> do
    (s, env, ts) <- inferPatterns ps
    Right (s, env, TTuple ts)

  -- コンストラクタパターン Just x, Pair a b
  PConstr con args -> do
    case lookupEnv builtinPatternEnv con of
      Nothing -> Left (InferOther ("Unknown constructor: " ++ con))
      Just scheme -> do
        tCon <- instantiate scheme
        inferPatternApp tCon args

  -- Cons パターン (x:xs)
  PCons p1 p2 -> do
    (s1, env1, t1) <- inferPattern p1
    (s2, env2, t2) <- inferPattern p2
    case unify (apply s2 t2) (TList t1) of
      Left _ -> Left (InferMismatch t2 (TList t1))
      Right s3 ->
        let s = composeSubst s3 (composeSubst s2 s1)
            env = mergeEnvs env1 env2
         in Right (s, env, apply s (TList t1))

  -- As パターン x@p
  PAs name p -> do
    (s1, env1, t1) <- inferPattern p
    let env2 = extendEnv env1 name (Forall [] t1)
    Right (s1, env2, t1)

inferPatternApp :: Type -> [Pattern] -> Either InferError (Subst, TypeEnv, Type)
inferPatternApp tCon [] =
  Right (emptySubst, emptyEnv, tCon)
inferPatternApp tCon (p : ps) = do
  (s1, env1, tArg) <- inferPattern p
  let alpha = TVar "t_app"
  case unify (apply s1 tCon) (TArrow tArg alpha) of
    Left _ -> Left (InferMismatch (apply s1 tCon) (TArrow tArg alpha))
    Right s2 -> do
      (s3, env2, tRes) <- inferPatternApp (apply s2 alpha) ps
      let s = composeSubst s3 (composeSubst s2 s1)
      let env = mergeEnvs env1 env2
      Right (s, env, apply s tRes)

inferPatterns :: [Pattern] -> Either InferError (Subst, TypeEnv, [Type])
inferPatterns [] = Right (emptySubst, emptyEnv, [])
inferPatterns (p : ps) = do
  (s1, env1, t1) <- inferPattern p
  (s2, env2, ts) <- inferPatterns ps
  let s = composeSubst s2 s1
  let env = mergeEnvs env1 env2
  Right (s, env, t1 : ts)
