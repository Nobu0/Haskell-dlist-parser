module TypeInference.SQLInfer where

import AST.Decl (Decl (..))
import AST.Expr (CaseAlt (..), Expr (..), Name)
import AST.Pattern (Pattern (..))
import AST.Type (Type (..))
import TypeInference.Error (InferError (..))
import TypeInference.Subst
import TypeInference.TypeEnv

data SQLInfo = SQLInfo
  { sqlText  :: String
  , sqlVars  :: [String]
  , sqlExprs :: [Expr]
  }

inferSQL :: String -> [Expr] -> SQLInfo
inferSQL sql args =
  SQLInfo
    { sqlText  = sql
    , sqlVars  = map getVarName args
    , sqlExprs = args
    }

getVarName :: Expr -> String
getVarName (EVar name) = name
getVarName _ = error "SQL arguments must be variables"

{-}
inferSQL ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  String ->
  [Expr] ->
  Either InferError (Subst, Type)
inferSQL inferExprFn env sql args = do
  inferred <- mapM (inferExprFn env) args
  return (emptySubst, TUnit)
-}

{-}
inferSQL :: TypeEnv -> String -> [Expr] -> Either InferError (Subst, Type)
inferSQL env sql args = do
  -- args の型を推論
  argTypes <- mapM (inferExpr env) args

  -- SQLParam 型クラスに属するかチェック（任意）
  mapM_ ensureSQLParam (map snd argTypes)

  -- SQL 文自体の型は Unit でよい
  return (emptySubst, TUnit)
-}
