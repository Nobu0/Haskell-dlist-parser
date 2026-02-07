module TypeInference.Infer
  ( InferError (..),
    inferExpr,
    inferDecl,
    inferProgram,
  )
where

import AST.Decl (Decl (..))
import AST.Expr (CaseAlt (..), Expr (..), Name)
import AST.Pattern (Pattern (..))
import AST.Type (Type (..))
-- import TypeInference.TypeEnv

import qualified Control.Exception as TypeInference
import Control.Monad (foldM)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Error (InferError (..))
import TypeInference.Subst
import TypeInference.TypeEnv
  ( Scheme (..),
    TypeEnv (..),
    applyEnv,
    emptyEnv,
    extendEnv,
    freeTypeVars,
    freeTypeVarsEnv,
    generalize,
    instantiate,
    lookupEnv,
  )
import TypeInference.Unify (UnifyError (..), unify)

-- inferExpr の返り値：型と代入
type InferResult = (Subst, Type)

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
{-# NOINLINE counter #-}

builtinPatternEnv :: TypeEnv
builtinPatternEnv =
  TypeEnv
    ( M.fromList
        [ ("Just", Forall ["a"] (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "a")))),
          ("Nothing", Forall ["a"] (TApp (TCon "Maybe") (TVar "a"))),
          (":", Forall ["a"] (TArrow (TVar "a") (TArrow (TList (TVar "a")) (TList (TVar "a"))))),
          ("[]", Forall ["a"] (TList (TVar "a")))
        ]
    )

builtinOps :: [(String, Scheme)]
builtinOps =
  [ ("++", Forall [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "String")))),
    ("+", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
    ("*", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
  ]

freshTypeVar :: Either InferError Type
freshTypeVar =
  Right
    ( TVar
        ( "t"
            ++ show
              ( unsafePerformIO
                  ( do
                      n <- readIORef counter
                      writeIORef counter (n + 1)
                      return n
                  )
              )
        )
    )

inferBindings :: TypeEnv -> [(Pattern, Expr)] -> Either InferError (Subst, TypeEnv)
inferBindings env [] =
  Right (emptySubst, emptyEnv)
inferBindings env ((pat, e) : rest) = do
  -- パターン推論
  (sPat, envPat, tPat) <- inferPattern pat

  -- 式の推論
  (sExpr, tExpr) <- inferExpr (applyEnv sPat env) e

  -- パターン型と式の型を unify（ここがエラーの原因だった）
  sUnify <- case unify (apply sExpr tPat) tExpr of
    Left uerr -> Left (InferUnifyError uerr)
    Right s -> Right s

  let s = sUnify `composeSubst` sExpr `composeSubst` sPat
  let env' = applyEnv s env
  let envPat' = applyEnv s envPat

  -- 残りのバインディング
  (sRest, envRest) <- inferBindings env' rest

  let sFinal = sRest `composeSubst` s
  let envFinal = mergeEnvs envPat' envRest

  Right (sFinal, envFinal)

inferBranch :: TypeEnv -> Type -> Subst -> CaseAlt -> Either InferError (Subst, Type)
inferBranch env tScrut sScrut (CaseAlt pat expr) = do
  (sPat, envPat, tPat) <- inferPattern pat

  sUnify <- case unify (apply sPat tPat) (apply sPat tScrut) of
    Left uerr -> Left (InferUnifyError uerr)
    Right s -> Right s

  let s = sUnify `composeSubst` sPat `composeSubst` sScrut
  inferExpr (applyEnv s (mergeEnvs env envPat)) expr

unifyManyExpr :: [(Subst, Type)] -> Either InferError (Subst, Type)
unifyManyExpr [] = Left (InferOther "empty case")
unifyManyExpr ((s, t) : xs) = foldM step (s, t) xs
  where
    step (sAcc, tAcc) (sNext, tNext) = do
      sU <- case unify (apply sAcc tAcc) (apply sAcc tNext) of
        Left uerr -> Left (InferUnifyError uerr)
        Right s -> Right s
      let sFinal = sU `composeSubst` sNext `composeSubst` sAcc
      Right (sFinal, apply sFinal tAcc)

unifyList :: Type -> [(Subst, Type)] -> Either InferError Subst
unifyList t [] = Right emptySubst
unifyList t ((s, tElem) : rest) = do
  sU <- case unify (apply s t) tElem of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s' = sU `composeSubst` s
  unifyList (apply s' t) rest

-- 式の型推論（まだ中身は空）
-- inferExpr :: TypeEnv -> Expr -> Either InferError InferResult
inferExpr :: TypeEnv -> Expr -> Either InferError (Subst, Type)
inferExpr env (ELetBlock binds body) = do
  (sBinds, envBinds) <- inferBindings env binds
  (sBody, tBody) <- inferExpr (applyEnv sBinds (mergeEnvs env envBinds)) body
  Right (sBody `composeSubst` sBinds, tBody)

-- 1. 変数
inferExpr env (EVar x) =
  case lookupEnv env x of
    Nothing -> Left (InferUnboundVariable x)
    Just sigma -> do
      t <- instantiate sigma
      Right (emptySubst, t)

-- 2. 整数リテラル
inferExpr env (EInt _) =
  Right (emptySubst, TCon "Int")
-- 3. ブールリテラル
inferExpr env (EBool _) =
  Right (emptySubst, TCon "Bool")
-- 4. 文字列リテラル
inferExpr env (EString _) =
  Right (emptySubst, TCon "String")
-- 5. 関数適用（HM 型推論の核）
inferExpr env (EApp e1 e2) = do
  (s1, t1) <- inferExpr env e1
  (s2, t2) <- inferExpr (applyEnv s1 env) e2
  tv <- freshTypeVar
  case unify (apply s2 t1) (TArrow t2 tv) of
    Left _ ->
      Left (InferMismatch (apply s2 t1) (TArrow t2 tv))
    Right s3 ->
      let s = s3 `composeSubst` s2 `composeSubst` s1
       in Right (s, apply s3 tv)

-- 6. ラムダ式
inferExpr env (ELam pat body) = do
  (s1, env1, tPat) <- inferPattern pat
  (s2, tBody) <- inferExpr (applyEnv s1 (mergeEnvs env env1)) body
  let s = s2 `composeSubst` s1
  Right (s, TArrow (apply s tPat) tBody)

-- 7. let 式
inferExpr env (ELet pat e1 e2) = do
  -- パターン推論
  (sPat, envPat, tPat) <- inferPattern pat

  -- e1 の推論
  (s1, t1) <- inferExpr (applyEnv sPat env) e1

  -- パターン型と e1 の型を unify
  s2 <- case unify (apply s1 tPat) t1 of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su

  -- ここまでの置換をまとめる
  let s = s2 `composeSubst` s1 `composeSubst` sPat

  -- パターンから得た環境を適用
  let env' = mergeEnvs (applyEnv s env) (applyEnv s envPat)

  -- e2 の推論
  (s3, t2) <- inferExpr env' e2

  -- 全体の置換
  let sFinal = s3 `composeSubst` s

  Right (sFinal, t2)

-- 8. if 式
inferExpr env (EIf cond eThen eElse) = do
  (s1, tCond) <- inferExpr env cond

  sBool <- case unify tCond (TCon "Bool") of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su

  let env1 = applyEnv (sBool `composeSubst` s1) env

  (s2, tThen) <- inferExpr env1 eThen
  (s3, tElse) <- inferExpr (applyEnv s2 env1) eElse

  s4 <- case unify (apply s3 tThen) tElse of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su

  let s = s4 `composeSubst` s3 `composeSubst` s2 `composeSubst` s1
  Right (s, apply s4 tElse)

-- 9. case 式
inferExpr env (ECase scrut branches) = do
  (sScrut, tScrut) <- inferExpr env scrut
  results <- mapM (inferBranch env tScrut sScrut) branches
  unifyManyExpr results

-- 10. タプル
inferExpr env (ETuple es) = do
  inferred <- mapM (inferExpr env) es
  let substs = map fst inferred
  let types = map snd inferred
  let s = foldl composeSubst emptySubst substs
  Right (s, TTuple (map (apply s) types))

-- 11. リスト
inferExpr env (EList es) = do
  inferred <- mapM (inferExpr env) es
  case inferred of
    [] -> do
      tv <- freshTypeVar
      Right (emptySubst, TList tv)
    ((s1, t1) : rest) -> do
      sRest <- unifyList t1 rest
      let s = sRest `composeSubst` s1
      Right (s, TList (apply s t1))

-- 宣言の型推論（まだ骨格だけ）
inferDecl :: TypeEnv -> Decl -> Either InferError (TypeEnv, Subst)
inferDecl env decl = case decl of
  DeclTypeSig name ty ->
    let scheme = Forall [] ty
     in Right (extendEnv env name scheme, emptySubst)
  DeclFun name pats body -> do
    -- パターンごとに型推論
    (sPats, envPats, argTypes) <- inferPatterns pats

    -- パターンで拡張した環境で body を推論
    (sBody, tBody) <- inferExpr (applyEnv sPats (mergeEnvs env envPats)) body

    let funType = foldr TArrow tBody argTypes
    let s = composeSubst sBody sPats
    let scheme = generalizeInfer env (apply s funType)

    Right (extendEnv env name scheme, s)
  DeclValue pat expr ->
    Left (InferOther "DeclValue not implemented yet")
  _ ->
    Right (env, emptySubst)

-- プログラム全体の型推論
inferProgram :: TypeEnv -> [Decl] -> Either InferError TypeEnv
inferProgram env decls = do
  let groups = groupDecls decls
  foldM inferGroup env (M.toList groups)

inferGroup :: TypeEnv -> (Name, [Decl]) -> Either InferError TypeEnv
inferGroup env (name, clauses) = do
  -- 仮の型を環境に入れる（再帰対応）
  let tempType = TVar ("t_fun_" ++ name)
  let envTemp = extendEnv env name (Forall [] tempType)

  -- 各 clause の型を推論
  inferred <- mapM (inferClause envTemp) clauses
  let funTypes = [apply s t | (s, t) <- inferred]

  -- unify して 1 つの型にまとめる
  s <- unifyMany funTypes
  let finalType = apply s (head funTypes)

  -- generalize
  let scheme = generalizeInfer env finalType

  Right (extendEnv env name scheme)

inferClause :: TypeEnv -> Decl -> Either InferError (Subst, Type)
inferClause env (DeclFun _ pats body) = do
  (sPats, envPats, argTypes) <- inferPatterns pats
  let env' = mergeEnvs env envPats
  (sBody, tBody) <- inferExpr (applyEnv sPats env') body
  let s = composeSubst sBody sPats

  -- ★ 関数型をここで作る
  let funType = foldr TArrow tBody argTypes

  Right (s, funType)

freshVar :: Int -> Type
freshVar n = TVar ("t" ++ show n)

inferPattern :: Pattern -> Either InferError (Subst, TypeEnv, Type)
inferPattern pat = case pat of
  -- 変数パターン
  PVar v ->
    let t = TVar ("t_" ++ v)
        env = extendEnv emptyEnv v (Forall [] t)
     in Right (emptySubst, env, t)
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
    Right (s, env, TApp (TCon ("Tuple" ++ show (length ts))) (foldl1 TApp ts))

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

  -- PApp は PConstr と同じ扱いで OK
  PApp p ps -> do
    (s1, env1, tFun) <- inferPattern p
    inferPatternApp tFun ps

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

mergeEnvs :: TypeEnv -> TypeEnv -> TypeEnv
mergeEnvs (TypeEnv e1) (TypeEnv e2) =
  TypeEnv (M.union e1 e2)

inferPatterns :: [Pattern] -> Either InferError (Subst, TypeEnv, [Type])
inferPatterns [] = Right (emptySubst, emptyEnv, [])
inferPatterns (p : ps) = do
  (s1, env1, t1) <- inferPattern p
  (s2, env2, ts) <- inferPatterns ps
  let s = composeSubst s2 s1
  let env = mergeEnvs env1 env2
  Right (s, env, t1 : ts)

generalizeInfer :: TypeEnv -> Type -> Scheme
generalizeInfer env t =
  let vars = nub (freeTypeVars t \\ freeTypeVarsEnv env)
   in Forall vars t

groupDecls :: [Decl] -> M.Map Name [Decl]
groupDecls decls =
  M.fromListWith (++) [(name, [d]) | d@(DeclFun name _ _) <- decls]

unifyMany :: [Type] -> Either InferError Subst
unifyMany [] = Right emptySubst
unifyMany (t : ts) =
  foldM
    ( \sacc t' ->
        case unify (apply sacc t) (apply sacc t') of
          Left _ -> Left (InferMismatch (apply sacc t) (apply sacc t'))
          Right s -> Right (composeSubst s sacc)
    )
    emptySubst
    ts
