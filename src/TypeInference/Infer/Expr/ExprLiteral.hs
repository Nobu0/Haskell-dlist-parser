module TypeInference.Infer.Expr.ExprLiteral
  ( inferInt,
    inferBool,
    inferString,
    inferTuple,
    inferList,
  )
where

import AST.Expr
import AST.Type
import Control.Monad (mapM)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferInt = Right (emptySubst, TCon "Int")

inferBool = Right (emptySubst, TCon "Bool")

inferString = Right (emptySubst, TCon "String")

inferTuple ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  [Expr] ->
  Either InferError (Subst, Type)
inferTuple inferExprFn env es = do
  inferred <- mapM (inferExprFn env) es
  let substs = map fst inferred
  let types = map snd inferred
  let s = foldl composeSubst emptySubst substs
  Right (s, TTuple (map (apply s) types))

inferList ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  [Expr] ->
  Either InferError (Subst, Type)
inferList inferExprFn env es = do
  inferred <- mapM (inferExprFn env) es
  case inferred of
    [] -> do
      tv <- freshTypeVar
      Right (emptySubst, TList tv)
    ((s1, t1) : rest) -> do
      sRest <- unifyList t1 rest
      let s = sRest `composeSubst` s1
      Right (s, TList (apply s t1))

unifyList :: Type -> [(Subst, Type)] -> Either InferError Subst
unifyList t [] = Right emptySubst
unifyList t ((s, tElem) : rest) = do
  sU <- case unify (apply s t) tElem of
    Left uerr -> Left (InferUnifyError uerr)
    Right su -> Right su
  let s' = sU `composeSubst` s
  unifyList (apply s' t) rest
