module TypeInference.Infer.Expr.ExprBinOp (inferBinOp) where

import AST.BinOp (BinOp (BinOpAdd))
import AST.Expr (BinOp (..), Expr (..))
import AST.Type
import Control.Monad.Trans.Class
import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)
import Utils.MyTrace

inferBinOp ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  BinOp ->
  Expr ->
  Expr ->
  InferM (Subst, Type)
inferBinOp inferExprFn env op e1 e2 = do
  -- myTraceE ("inferBinOp: " ++ show op ++ " with " ++ show e1 ++ " and " ++ show e2)
  (s1, t1) <- inferExprFn env e1
  (s2, t2) <- inferExprFn (applyEnv s1 env) e2
  let s12 = s2 `composeSubst` s1
  (arg1, arg2, result) <- case op of
    BinOpAdd -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpSub -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpMul -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpDiv -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpAnd -> return (TCon "Bool", TCon "Bool", TCon "Bool")
    BinOpOr -> return (TCon "Bool", TCon "Bool", TCon "Bool")
    BinOpEq -> do tv <- freshTypeVar; return (tv, tv, TCon "Bool")
    BinOpNeq -> do tv <- freshTypeVar; return (tv, tv, TCon "Bool")
    BinOpLt -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpGt -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpLe -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpGe -> return (TCon "Int", TCon "Int", TCon "Bool")

  sA <- lift $ first InferUnifyError $ unify (apply s12 t1) arg1
  sB <- lift $ first InferUnifyError $ unify (apply sA (apply s12 t2)) (apply sA arg2)
  let s = sB `composeSubst` sA `composeSubst` s12
  return (s, apply s result)

{-}
inferBinOp ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  BinOp ->
  Expr ->
  Expr ->
  Either InferError (Subst, Type)
inferBinOp inferExprFn env op e1 e2 = do
  (s1, t1) <- inferExprFn env e1
  (s2, t2) <- inferExprFn (applyEnv s1 env) e2
  let s12 = s2 `composeSubst` s1
  (arg1, arg2, result) <- case op of
    BinOpAdd -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpSub -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpMul -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpDiv -> return (TCon "Int", TCon "Int", TCon "Int")
    BinOpAnd -> return (TCon "Bool", TCon "Bool", TCon "Bool")
    BinOpOr -> return (TCon "Bool", TCon "Bool", TCon "Bool")
    BinOpEq -> do
      tv <- freshTypeVar
      return (tv, tv, TCon "Bool")
    BinOpNeq -> do
      tv <- freshTypeVar
      return (tv, tv, TCon "Bool")
    BinOpLt -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpGt -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpLe -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpGe -> return (TCon "Int", TCon "Int", TCon "Bool")
  s3 <- case unify (apply s12 t1) arg1 of
    Left uerr -> Left (InferUnifyError uerr)
    Right sA -> case unify (apply sA (apply s12 t2)) (apply sA arg2) of
      Left uerr -> Left (InferUnifyError uerr)
      Right sB -> Right (sB `composeSubst` sA)
  let s = s3 `composeSubst` s12
  Right (s, apply s result)
-}
