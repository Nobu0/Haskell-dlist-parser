module TypeInference.Infer.Expr.ExprDispatch (inferExpr) where

import AST.Expr
import AST.Type
import AST.Type (Type (..)) -- これで TFun などのコンストラクタが使えるようになる
import TypeInference.Error
import TypeInference.Infer.Expr.ExprApp (inferApp)
import TypeInference.Infer.Expr.ExprBinOp (inferBinOp)
import TypeInference.Infer.Expr.ExprCase (inferCase)
import TypeInference.Infer.Expr.ExprDo (inferDo)
import TypeInference.Infer.Expr.ExprIf (inferIf)
import TypeInference.Infer.Expr.ExprLet (inferLet, inferLetBlock, inferWhere)
import TypeInference.Infer.Expr.ExprLiteral (inferBool, inferInt, inferList, inferString, inferTuple)
import TypeInference.Infer.Expr.ExprSQL (inferSQL)
import TypeInference.Subst
import TypeInference.TypeEnv

-- 他の構文モジュールもここに import
inferExpr :: TypeEnv -> Expr -> Either InferError (Subst, Type)
inferExpr env (EVar name) =
  case lookupEnv env name of
    Nothing -> Left (InferUnboundVariable name)
    Just sigma -> do
      t <- instantiate sigma
      Right (emptySubst, t)
-- AST で定義された型で分岐 ここが欠損すると型推論ができなくなる
inferExpr env expr = case expr of
  ELet pat e1 e2 -> inferLet inferExpr env pat e1 e2
  ELetBlock binds body -> inferLetBlock inferExpr env binds body
  EWhere e binds -> inferWhere inferExpr env e binds
  EIf c t f -> inferIf inferExpr env c t f
  EDo stmts -> inferDo inferExpr env stmts
  ECase scrut alts -> inferCase inferExpr env scrut alts
  EApp e1 e2 -> inferApp inferExpr env e1 e2
  EBinOp op e1 e2 -> inferBinOp inferExpr env op e1 e2
  -- リテラル
  EInt _ -> inferInt
  EBool _ -> inferBool
  EString _ -> inferString
  ETuple es -> inferTuple inferExpr env es
  EList es -> inferList inferExpr env es
  -- 拡張タイプ SQL
  ESQL _ params -> inferSQL inferExpr env params

{-}
  ELetBlock binds body -> inferLetBlock env binds body
  ELet pat e1 e2 -> inferLet env pat e1 e2
  EWhere e binds -> inferWhere env e binds
  ECase scrut alts -> inferCase env scrut alts
  EApp e1 e2 -> inferApp env e1 e2
  ELam pat body -> inferLam env pat body
  EDo stmts -> inferDo env stmts
  EBinOp op e1 e2 -> inferBinOp env op e1 e2
  EInt _ -> inferInt
  EBool _ -> inferBool
  EString _ -> inferString
  ETuple es -> inferTuple env es
  EList es -> inferList env es
  ESQL _ params -> inferSQL env params
-}
