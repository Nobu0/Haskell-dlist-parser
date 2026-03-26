module TypeInference.Infer.Expr.ExprLiteral
  ( inferInt,
    inferBool,
    inferString,
    inferChar,
    inferTuple,
    inferList,
  )
where

import AST.Expr
-- import AST.Type
import Control.Monad (mapM)
import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferInt :: InferM (Subst, Type)
inferInt = return (emptySubst, TCon "Int")

inferBool :: InferM (Subst, Type)
inferBool = return (emptySubst, TCon "Bool")

inferString :: InferM (Subst, Type)
inferString = return (emptySubst, TCon "String")

inferChar :: InferM (Subst, Type)
inferChar = return (emptySubst, TCon "Char")

inferTuple ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [Expr] ->
  InferM (Subst, Type)
inferTuple inferExprFn env es = do
  inferred <- mapM (inferExprFn env) es
  let substs = map fst inferred
  let types = map snd inferred
  let s = foldl composeSubst emptySubst substs
  return (s, TTuple (map (apply s) types))

inferList ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  [Expr] ->
  InferM (Subst, Type)
inferList inferExprFn env es = do
  inferred <- mapM (inferExprFn env) es
  case inferred of
    [] -> do
      tv <- freshTypeVar
      return (emptySubst, TList tv)
    ((s1, t1) : rest) -> do
      sRest <- unifyList t1 rest
      let s = sRest `composeSubst` s1
      return (s, TList (apply s t1))

unifyList :: Type -> [(Subst, Type)] -> InferM Subst
unifyList t [] = return emptySubst
unifyList t ((s, tElem) : rest) = do
  sU <- lift $ first InferUnifyError (unify (apply s t) tElem)
  let s' = sU `composeSubst` s
  unifyList (apply s' t) rest
