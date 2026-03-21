module TypeInference.Infer.Expr
  ( inferExpr,
    inferProgram,
    inferDecl,
  )
where

import AST.Decl (Decl (..), FunClause (..))
import AST.Expr
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
import AST.Pattern (Pattern (..))
-- import AST.Type
-- import AST.Type (Type (..))

import qualified AST.Type as AST
import qualified Control.Exception as TypeInference
-- (setTrace)
-- import TypeInference.Types

import Control.Monad (foldM, replicateM, when, zipWithM)
import Data.Bifunctor (first)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprDispatch (inferExpr)
import TypeInference.Infer.Expr.ExprSQL
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.Type
import qualified TypeInference.Type as TI
import TypeInference.TypeEnv
import qualified TypeInference.TypeEnv as TypeEnv
import qualified TypeInference.Types as Types
import TypeInference.Unify (unify)
import Utils.MyTrace

type Infer a = Either InferError a

inferProgram :: TypeEnv -> [Decl] -> InferM TypeEnv
inferProgram env decls = do
  let groups = groupDecls decls
  myTraceE ("<< inferProgram: groups " ++ show groups)
  foldM
    ( \env (name, (clauses, mTy)) ->
        let mTy' = fmap convertType mTy
         in do
              (_s, env') <- inferFunGroup env name clauses mTy'
              return env'
    )
    env
    (M.toList groups)

inferFunGroup :: TypeEnv -> String -> [FunClause] -> Maybe TI.Type -> InferM (Subst, TypeEnv)
inferFunGroup env name clauses mTy = do
  myTraceE ("<< inferGroup: clauses " ++ show clauses)
  -- 1. 関数の型（明示 or fresh）を決定
  ty <- case mTy of
    Just t -> return t
    Nothing -> freshType

  -- 2. 関数名と型スキームを環境に追加（再帰対応！）
  -- let scheme = TypeEnv.generalize env ty
  let scheme = Forall [] ty
      env' = extendEnv env name scheme

  myTraceE $ "<< inferFunGroup: extended env with " ++ name ++ " :: " ++ show scheme

  -- 3. 関数節を推論（新しい環境で）
  (s, tBody) <- inferFunClauses env' clauses

  -- 4. 型の整合性チェック（必要なら unify ty と tBody）
  sUnify <- case unify (apply s tBody) (apply s ty) of
    Left err -> lift $ Left (InferUnifyError err)
    Right su -> return su

  let sFinal = sUnify `composeSubst` s
      schemeFinal = TypeEnv.generalize (applyEnv sFinal env) (apply sFinal tBody)
      envFinal = extendEnv env name schemeFinal

  return (sFinal, envFinal)

inferFunClauses :: TypeEnv -> [FunClause] -> InferM (Subst, Type)
inferFunClauses env clauses = do
  -- 1. 引数の数を確認
  let arity = case clauses of
        (FunClause pats _ _ _ : _) -> length pats
        _ -> 0

  -- 2. 共通の引数型変数を生成
  argTypes <- replicateM arity freshTypeVar
  retType <- freshTypeVar
  let expectedType = foldr TArrow retType argTypes

  -- 3. 関数名を仮に使うために環境に追加（再帰対応）
  -- let scheme = Forall [] (convertType (env expectedType))
  let scheme = TypeEnv.generalize env expectedType
      env' = extendEnv env "__self__" scheme -- "__self__" は仮の関数名（必要に応じて変更）

  -- 4. 各節を推論（共通の引数型を使う）
  results <- mapM (inferFunClauseWithArgs env' argTypes retType) clauses
  let (subs, _) = unzip results
  s <- foldM composeSubstM emptySubst subs
  return (s, apply s expectedType)

unifyManyM :: [Type] -> InferM Type
unifyManyM [] = lift $ Left (InferOther "Cannot unify empty type list")
unifyManyM (t : ts) =
  if all (== t) ts
    then return t
    else lift $ Left (InferMismatchGroup (t : ts))

inferFunClauseWithArgs :: TypeEnv -> [Type] -> Type -> FunClause -> InferM (Subst, Type)
inferFunClauseWithArgs env argTypes retType (FunClause pats _mbGuards (Just body) _mbWhere) = do
  -- 引数の数とパターンの数が一致しているか確認
  when (length argTypes /= length pats) $
    lift $
      Left (InferOther "Pattern count does not match argument types")

  -- 各パターンに対応する型を指定して推論
  (subs, envs, _) <- unzip3 <$> zipWithM inferPatternWith argTypes pats
  let sPats = foldr composeSubst emptySubst subs
      envPats = foldr mergeEnvs emptyEnv envs
      env' = mergeEnvs env (applyEnv sPats envPats)

  -- 本体を推論
  (sBody, tBody) <- inferExpr env' body

  -- 戻り値の型と unify
  sRet <- lift $ first InferUnifyError $ unify (apply sBody retType) tBody

  let s = sRet `composeSubst` sBody `composeSubst` sPats
      funType = foldr TArrow tBody argTypes

  return (s, funType)
inferFunClauseWithArgs _ _ _ (FunClause _ _ Nothing _) =
  lift $ Left (InferOther "Function clause missing body")

inferPatternWith :: Type -> Pattern -> InferM (Subst, TypeEnv, Type)
inferPatternWith expectedTy pat = do
  (s, env, ty) <- inferPattern pat
  s' <- lift $ first InferUnifyError $ unify ty expectedTy
  let sFinal = s' `composeSubst` s
  return (sFinal, applyEnv sFinal env, apply sFinal ty)

composeSubstM :: Subst -> Subst -> InferM Subst
composeSubstM s1 s2 = return (composeSubst s1 s2)

inferClause :: TypeEnv -> Decl -> InferM (Subst, Type)
inferClause env (DeclFunGroup _ clauses) = do
  myTraceE ("<< inferClause: env " ++ show env)
  results <- mapM (inferFunClause env) clauses
  myTraceE ("<< inferClause: results " ++ show results)
  let (substs, types) = unzip results
  myTraceE ("<< inferClause: substs " ++ show substs)
  let s = foldr composeSubst emptySubst substs
  myTraceE ("<< inferClause: s " ++ show s ++ " types " ++ show types)
  -- すべての型が一致するか確認（ここでは最初の型に合わせる）
  case types of
    [] -> lift $ Left (InferOther "Empty function group")
    (t : ts) ->
      if all (== t) ts
        then return (s, t)
        else lift $ Left (InferMismatchGroup types)
inferClause _ decl =
  lift $ Left (InferOther ("Unsupported declaration: " ++ show decl))

inferFunClause :: TypeEnv -> FunClause -> InferM (Subst, Type)
inferFunClause env (FunClause pats _mbGuards (Just body) _mbWhere) = do
  (sPats, envPats, argTypes) <- inferPatterns pats
  let envPatApplied = applyEnv sPats envPats
  let env'' = mergeEnvs env envPatApplied
  (sBody, tBody) <- inferExpr env'' body
  myTraceE ("<< inferFunClause: sBody " ++ show sBody ++ " tBody " ++ show tBody)
  -- let env' = mergeEnvs env envPats
  -- (sBody, tBody) <- inferExpr (applyEnv sPats env') body
  let s = composeSubst sBody sPats
  let funType = foldr TArrow tBody argTypes
  return (s, funType)
inferFunClause _ (FunClause _ _ Nothing _) =
  lift $ Left (InferOther "Function clause missing body")

-- 宣言の型推論（まだ骨格だけ）
inferDecl :: TypeEnv -> Decl -> InferM (TypeEnv, Subst)
inferDecl env decl = do
  myTraceE ("<< inferDecl: decl " ++ show decl)
  case decl of
    DeclTypeSig name ty ->
      let scheme = Forall [] (convertType ty)
       in return (extendEnv env name scheme, emptySubst)
    DeclFunGroup name [FunClause pats _guards (Just body) _where] -> do
      (sPats, envPats, argTypes) <- inferPatterns pats
      let env' = mergeEnvs env envPats
      (sBody, tBody) <- inferExpr (applyEnv sPats env') body
      let funType = foldr TArrow tBody argTypes
      let s = composeSubst sBody sPats
      let scheme = generalizeInfer env (apply s funType)
      return (extendEnv env name scheme, s)
    DeclFunGroup _ _ ->
      lift $ Left (InferOther "Guarded or multiple-clause functions not yet supported")
    DeclValue pat expr ->
      lift $ Left (InferOther "DeclValue not implemented yet")
    _ ->
      return (env, emptySubst)

lookupTypeSig :: Name -> [Decl] -> Maybe Type
lookupTypeSig name decls =
  case [ty | DeclTypeSig n ty <- decls, n == name] of
    (ty : _) -> Just (convertType ty)
    [] -> Nothing

generalize :: TypeEnv -> Type -> Scheme
generalize env t =
  let vars = freeTypeVars t \\ freeTypeVarsEnv env
   in Forall (nub vars) t

convertType :: AST.Type -> TI.Type
convertType astTy = case astTy of
  AST.TVar v -> TI.TVar v
  AST.TCon c -> TI.TCon c
  AST.TArrow a b -> TI.TArrow (convertType a) (convertType b)
  AST.TList t -> TI.TList (convertType t)
  AST.TApp f x -> TI.TApp (convertType f) (convertType x)
  AST.TTuple ts -> TI.TTuple (map convertType ts)
  AST.TUnit -> TI.TUnit
  AST.TBinOp op a b -> TI.TBinOp op (convertType a) (convertType b)
  AST.TConstraint cs t -> TI.TConstraint (map convertConstraint cs) (convertType t)
  AST.TForall vs t -> TI.TForall vs (convertType t)

convertConstraint :: AST.Constraint -> TI.Constraint
convertConstraint (AST.Constraint cls ts) = TI.Constraint cls (map convertType ts)
