inferExpr env (EApp (EApp (EVar "+") e1) e2) = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  unify t1 TInt
  unify t2 TInt
  return TInt

inferBinOp :: Type -> Type -> Type -> Infer Type
inferBinOp expected1 expected2 result = do
  unify expected1 ...
  unify expected2 ...
  return result


inferExpr env (EApp e1 e2) = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  tv <- freshTypeVar
  unify t1 (TArrow t2 tv)
  return tv

inferApp :: TypeEnv -> Expr -> Expr -> Infer Type
inferApp env e1 e2 = do
  t1 <- inferExpr env e1
  t2 <- inferExpr env e2
  tv <- freshTypeVar
  unify t1 (TArrow t2 tv)
  return tv


inferPattern' (PVar name) expectedType = do
  return (nullSubst, extendEnv emptyEnv name (Forall [] expectedType))

bindVar :: Name -> Type -> TypeEnv
bindVar name ty = extendEnv emptyEnv name (Forall [] ty)


let t = apply s1 expectedType
    env2 = extendEnv env1 name (Forall [] t)

bindAsPattern :: Name -> Type -> TypeEnv -> TypeEnv
bindAsPattern name ty env = extendEnv env name (Forall [] ty)


inferExpr env (EVar name) = do
  scheme <- lookupEnv env name
  instantiate scheme

lookupAndInstantiate :: TypeEnv -> Name -> Infer Type
lookupAndInstantiate env name = do
  scheme <- lookupEnv env name
  instantiate scheme
