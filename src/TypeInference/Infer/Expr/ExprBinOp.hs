module TypeInference.Infer.Expr.ExprBinOp (inferBinOp, inferOpSectionL, inferOpSectionR) where

import AST.BinOp (BinOp (BinOpAdd))
import AST.Expr (BinOp (..), Expr (..))
-- import AST.Type
import Control.Monad.Trans.Class
import Data.Bifunctor (first)
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
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
inferBinOp inferExprFn env BinOpCompose e1 e2 = do
  (s1, t1) <- inferExprFn env e1
  (s2, t2) <- inferExprFn (applyEnv s1 env) e2
  let s12 = s2 `composeSubst` s1
  tvA <- freshTypeVar
  tvB <- freshTypeVar
  tvC <- freshTypeVar
  sF <- lift $ first InferUnifyError $ unify (apply s12 t1) (TArrow tvB tvC)
  sG <- lift $ first InferUnifyError $ unify (apply sF (apply s12 t2)) (TArrow tvA (apply sF tvB))
  let s = sG `composeSubst` sF `composeSubst` s12
  return (s, apply s (TArrow tvA tvC))
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
    BinOpEq -> do tv <- freshTypeVar; return (tv, tv, TCon "Bool")
    BinOpNeq -> do tv <- freshTypeVar; return (tv, tv, TCon "Bool")
    BinOpLt -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpGt -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpLe -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpGe -> return (TCon "Int", TCon "Int", TCon "Bool")
    BinOpCons -> do
      tv <- freshTypeVar
      return (tv, TList tv, TList tv)
    BinOpConcat -> do
      tv <- freshTypeVar
      return (TList tv, TList tv, TList tv)
  sA <- lift $ first InferUnifyError $ unify (apply s12 t1) arg1
  sB <- lift $ first InferUnifyError $ unify (apply sA (apply s12 t2)) (apply sA arg2)
  let s = sB `composeSubst` sA `composeSubst` s12
  return (s, apply s result)

inferOpSectionL ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  BinOp ->
  Expr ->
  InferM (Subst, Type)
inferOpSectionL infer env op e = do
  (s1, t2) <- infer env e
  opType <- lookupBinOpType env op
  tv1 <- freshTypeVar
  tvRes <- freshTypeVar
  case unify (TArrow tv1 (TArrow t2 tvRes)) opType of
    Left err -> lift $ Left (InferUnifyError err)
    Right s2 -> do
      let finalType = apply s2 (TArrow tv1 tvRes)
      return (s2 `composeSubst` s1, finalType)

inferOpSectionR ::
  (TypeEnv -> Expr -> InferM (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  BinOp ->
  InferM (Subst, Type)
inferOpSectionR infer env e op = do
  (s1, t1) <- infer env e
  opType <- lookupBinOpType env op
  tv2 <- freshTypeVar
  tvRes <- freshTypeVar
  case unify (TArrow t1 (TArrow tv2 tvRes)) opType of
    Left err -> lift $ Left (InferUnifyError err)
    Right s2 -> do
      let finalType = apply s2 (TArrow tv2 tvRes)
      return (s2 `composeSubst` s1, finalType)

lookupBinOpType :: TypeEnv -> BinOp -> InferM Type
lookupBinOpType env op = do
  let name = binOpName op
  case lookupEnv env name of
    Nothing -> lift $ Left (InferUnboundVariable name)
    Just sigma -> instantiateM sigma

instantiateM :: Scheme -> InferM Type
instantiateM sigma = lift (instantiate sigma)

binOpName :: BinOp -> String
binOpName BinOpAdd = "+"
binOpName BinOpSub = "-"
binOpName BinOpMul = "*"
binOpName BinOpDiv = "/"
binOpName BinOpEq = "=="
binOpName BinOpNeq = "/="
binOpName BinOpLe = "<="
binOpName BinOpGe = ">="
binOpName BinOpLt = "<"
binOpName BinOpGt = ">"
binOpName BinOpAnd = "&&"
binOpName BinOpOr = "||"
