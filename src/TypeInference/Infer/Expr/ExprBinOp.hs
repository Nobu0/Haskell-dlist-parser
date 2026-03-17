module TypeInference.Infer.Expr.ExprBinOp (inferBinOp) where

import AST.Expr (BinOp (..), Expr (..))
import AST.Type
import TypeInference.Error
import TypeInference.Infer.Core
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

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
    Add -> pure (TCon "Int", TCon "Int", TCon "Int")
    Sub -> pure (TCon "Int", TCon "Int", TCon "Int")
    Mul -> pure (TCon "Int", TCon "Int", TCon "Int")
    Div -> pure (TCon "Int", TCon "Int", TCon "Int")
    And -> pure (TCon "Bool", TCon "Bool", TCon "Bool")
    Or -> pure (TCon "Bool", TCon "Bool", TCon "Bool")
    Eq -> do
      tv <- freshTypeVar
      pure (tv, tv, TCon "Bool")
    Neq -> do
      tv <- freshTypeVar
      pure (tv, tv, TCon "Bool")
    Lt -> pure (TCon "Int", TCon "Int", TCon "Bool")
    Gt -> pure (TCon "Int", TCon "Int", TCon "Bool")
    Le -> pure (TCon "Int", TCon "Int", TCon "Bool")
    Ge -> pure (TCon "Int", TCon "Int", TCon "Bool")
  s3 <- case unify (apply s12 t1) arg1 of
    Left uerr -> Left (InferUnifyError uerr)
    Right sA -> case unify (apply sA (apply s12 t2)) (apply sA arg2) of
      Left uerr -> Left (InferUnifyError uerr)
      Right sB -> Right (sB `composeSubst` sA)
  let s = s3 `composeSubst` s12
  Right (s, apply s result)
