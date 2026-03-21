module TypeInference.Infer.Expr.ExprDispatch (inferExpr) where

import AST.Expr
-- import AST.Type
-- import AST.Type (Type (..)) -- これで TFun などのコンストラクタが使えるようになる

-- import Data.Map

-- import TypeInference.Type (Type (..))

import qualified AST.Type as AST
import Data.Bifunctor (first)
import qualified Data.Map as Map
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Infer.Expr.ExprApp (inferApp)
import TypeInference.Infer.Expr.ExprBinOp (inferBinOp, inferOpSectionL, inferOpSectionR)
import TypeInference.Infer.Expr.ExprCase (inferCase)
import TypeInference.Infer.Expr.ExprDo (inferDo)
import TypeInference.Infer.Expr.ExprIf (inferIf)
import TypeInference.Infer.Expr.ExprLet (inferLet, inferLetBlock, inferWhere)
import TypeInference.Infer.Expr.ExprLiteral (inferBool, inferInt, inferList, inferString, inferTuple)
import TypeInference.Infer.Expr.ExprSQL (inferSQL)
import TypeInference.Subst
-- import qualified TypeInference.Type as TI

import TypeInference.Type
import TypeInference.TypeEnv
import Utils.MyTrace

-- 他の構文モジュールもここに import
inferExpr :: TypeEnv -> Expr -> InferM (Subst, Type)
inferExpr env (EVar name) = do
  myTraceE ("<< inferExpr: env " ++ show env ++ " name " ++ show name)
  case lookupEnv env name of
    Nothing -> lift $ Left (InferUnboundVariable name)
    Just sigma -> do
      t <- lift $ instantiate sigma
      return (emptySubst, t)

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
  ETuple es -> inferTuple inferExpr env es
  EList es -> inferList inferExpr env es
  ESQL _ params -> inferSQL inferExpr env params
  EOpSectionL op e -> inferOpSectionL inferExpr env op e
  EOpSectionR e op -> inferOpSectionR inferExpr env e op
  ERecord fields -> inferRecord inferExpr env fields

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
