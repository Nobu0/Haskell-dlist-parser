module Language.TypeSystem.InferExpr (inferExpr) where

-- import qualified AST.Pattern as AP

import Control.Monad (foldM, forM)
import Control.Monad.Combinators (empty)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.BinOp (BinOp (..), binOpName)
import Language.TypeSystem.Class
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.Env
import Language.TypeSystem.EnvInstance
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
import Language.TypeSystem.InferM
import Language.TypeSystem.MyTrace
import Language.TypeSystem.Pattern
import qualified Language.TypeSystem.Pattern as TP
import Language.TypeSystem.PatternInfer
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax
import Language.TypeSystem.Unify

-- import TypeInference.Infer.Core (freshType)

-- | 式の型推論（ディスパッチ）
inferExpr :: Expr -> InferM (Subst, [Pred], Type)
inferExpr expr = do
  myTraceE ("[inferExpr] expr = " ++ show expr)
  case expr of
    -- inferExpr expr = case expr of
    EApp f x -> inferApp f x
    EVar x -> inferVar x
    EInt n -> inferInt n
    EBool b -> inferBool b
    EChar c -> inferChar c
    EString s -> inferString s
    EUnit -> inferUnit
    ELam ps body -> inferLam ps body
    ELet p e1 e2 -> inferLet p e1 e2
    EIf c t e -> inferIf c t e
    EAnn e t -> inferAnn e t
    ECase e alts -> inferCase e alts
    EList es -> inferList es
    ETuple es -> inferTuple es
    EBinOp o l r -> inferBinOp o l r
    EOpSectionL o e -> inferOpSectionL o e
    EOpSectionR e o -> inferOpSectionR e o
    ELetBlock bs e -> inferLetBlock bs e
    EWhere e bs -> inferWhere e bs
    ELambdaCase alts -> inferLambdaCase alts
    ERecord fields -> inferRecord (Map.toList fields)
    EFieldAccess e f -> inferFieldAccess e f
    ERecordUpdate e fs -> inferRecordUpdate e fs
    _ -> throwError $ OtherError ("inferExpr: unsupported expression form " ++ show expr)

inferRecord :: [(Name, Expr)] -> InferM (Subst, [Pred], Type)
inferRecord fields = do
  myTraceE ("[inferRecord] fields = " ++ show fields)
  inferred <-
    mapM
      ( \(name, expr) -> do
          (s, ps, t) <- inferExpr expr
          return (s, ps, name, t)
      )
      fields
  myTraceE ("[inferRecord] inferred = " ++ show inferred)
  let s = composeMany [s | (s, _, _, _) <- inferred]
      ps = concat [ps | (_, ps, _, _) <- inferred]
      fieldTypes = Map.fromList [(name, t) | (_, _, name, t) <- inferred]
  return (s, ps, TRecord (applySubst s fieldTypes) Nothing)

inferFieldAccess :: Expr -> Name -> InferM (Subst, [Pred], Type)
inferFieldAccess e field = do
  myTraceE ("[inferFieldAccess] e = " ++ show e ++ ", field = " ++ field)
  (s1, ps1, tRec) <- inferExpr e
  tv <- freshTypeVar
  rowVar <- freshTypeVar
  let row = rowVar
      expected = TRecord (Map.singleton field tv) (Just row)
  myTraceE ("[inferFieldAccess] row = " ++ show row ++ ", expected = " ++ show expected)
  s2 <- unify tRec expected
  return (s2 `composeSubst` s1, applySubst s2 ps1, applySubst s2 tv)

inferRecordUpdate :: Expr -> [(Name, Expr)] -> InferM (Subst, [Pred], Type)
inferRecordUpdate e updates = do
  myTraceE ("[inferRecordUpdate] updates = " ++ show updates)
  (s1, ps1, tRec) <- inferExpr e
  updateTypes <-
    mapM
      ( \(name, expr) -> do
          (s, ps, t) <- inferExpr expr
          return (s, ps, name, t)
      )
      updates
  let s = composeMany (map (\(s, _, _, _) -> s) updateTypes)
      ps = concatMap (\(_, ps, _, _) -> ps) updateTypes
      updateFields = Map.fromList [(name, t) | (_, _, name, t) <- updateTypes]
  tv <- freshTypeVar
  let expected = TRecord updateFields (Just tv)
  s2 <- unify (applySubst s tRec) expected
  let finalSubst = s2 `composeSubst` s
      finalPreds = applySubst finalSubst (ps ++ ps1)
      finalType = applySubst finalSubst tRec
  return (finalSubst `composeSubst` s1, finalPreds, finalType)

inferLambdaCase :: [CaseAlt] -> InferM (Subst, [Pred], Type)
inferLambdaCase alts = do
  myTraceE ("[inferLambdaCase] alts = " ++ show alts)
  tvArg <- freshTypeVar
  tvRes <- freshTypeVar
  baseEnv <- getEnv -- ✅ 元の環境を取得
  results <- forM alts $ \(CaseAlt pat body) -> do
    (s1, env1, tPat) <- inferPattern pat
    s2 <- unify tPat tvArg
    let s12 = s2 `composeSubst` s1
        mergedEnv = env1 `envMerge` baseEnv -- ✅ パターン束縛と元の環境をマージ
    putEnv (applySubst s12 mergedEnv)
    (s3, ps, tBody) <- inferExpr (applySubst s12 body)
    s4 <- unify (applySubst s3 tBody) (applySubst s3 tvRes)
    let s = composeMany [s4, s3, s2, s1]
    return (s, applySubst s ps)
  let (subs, predsList) = unzip results
      sAll = composeMany subs
      preds = concat predsList
  return (sAll, preds, TArrow (applySubst sAll tvArg) (applySubst sAll tvRes))

inferWhere :: Expr -> [(Pattern, Expr)] -> InferM (Subst, [Pred], Type)
inferWhere e ds = inferLetBlock ds e

inferValueDecls :: [(Pattern, Expr)] -> InferM (Subst, [Pred], TypeEnv)
inferValueDecls [] = do
  env <- getEnv
  return (emptySubst, [], env)
inferValueDecls ((pat, expr) : ds) = do
  (s1, ps1, t1) <- inferExpr expr
  env <- getEnv
  (s2, env', _) <- inferPattern pat
  putEnv (env' `envMerge` applySubst s2 env)
  (s3, ps2, env'') <- inferValueDecls ds
  return (s3 `composeSubst` s2 `composeSubst` s1, ps1 ++ ps2, env'')

inferLetBlock :: [(Pattern, Expr)] -> Expr -> InferM (Subst, [Pred], Type)
inferLetBlock decls body = do
  let valDecls = [(p, e) | (p, e) <- decls]
  env <- getEnv
  (s1, ps1, env') <- inferValueDecls valDecls
  putEnv env'
  (s2, ps2, tBody) <- inferExpr body
  putEnv env
  let s = s2 `composeSubst` s1
  return (s, ps1 ++ ps2, tBody)

inferOpSectionL o e = do
  x <- freshName
  let lam = ELam [PVar x] (EBinOp o (EVar x) e)
  inferExpr lam

inferOpSectionR e o = do
  x <- freshName
  let lam = ELam [PVar x] (EBinOp o e (EVar x))
  inferExpr lam

-- inferBinOp :: Name -> Expr -> Expr -> InferM (Subst, [Pred], Type)
inferBinOp :: BinOp -> Expr -> Expr -> InferM (Subst, [Pred], Type)
inferBinOp op l r = inferExpr (EApp (EApp (EVar (binOpName op)) l) r)

inferTuple :: [Expr] -> InferM (Subst, [Pred], Type)
inferTuple [] = return (emptySubst, [], TCon "Unit")
inferTuple es = do
  inferred <- mapM inferExpr es
  let (subs, predsList, types) = unzip3 inferred
      s = composeMany subs
      ps = concat predsList
  -- return (s, ps, map (applySubst s) types `TTuple`)
  return (s, ps, TTuple (map (applySubst s) types))

-- | 整数リテラルの型推論（Num a => a）
inferInt :: Int -> InferM (Subst, [Pred], Type)
-- inferInt _ = return (emptySubst, [], TCon "Int")
inferInt _ = do
  tv <- freshTypeVar
  let pred = IsIn "Num" tv
  addPred pred
  return (emptySubst, [pred], tv)

-- | 真偽値リテラルの型推論（Bool）
inferBool :: Bool -> InferM (Subst, [Pred], Type)
inferBool _ = return (emptySubst, [], TCon "Bool")

-- | 文字リテラルの型推論（Char）
inferChar :: Char -> InferM (Subst, [Pred], Type)
inferChar _ = return (emptySubst, [], TCon "Char")

-- | 文字列リテラルの型推論（[Char]）
inferString :: String -> InferM (Subst, [Pred], Type)
inferString _ = return (emptySubst, [], TApp (TCon "List") (TCon "Char"))

-- | 単位値の型推論（Unit）
inferUnit :: InferM (Subst, [Pred], Type)
inferUnit = return (emptySubst, [], TCon "Unit")

inferApp :: Expr -> Expr -> InferM (Subst, [Pred], Type)
-- inferApp f x = error "[inferApp] THIS SHOULD BE CALLED"
inferApp f x = do
  myTraceE ("[inferApp] f = " ++ show f ++ ", x = " ++ show x)
  (s1, ps1, t1) <- inferExpr f
  (s2, ps2, t2) <- inferExpr (applySubst s1 x)
  tv <- freshTypeVar
  let t1' = applySubst s2 t1
      expected = TArrow t2 tv
  myTraceE ("[App] t1 = " ++ show t1' ++ ", expected = " ++ show expected)
  s3 <- unify t1' expected
  let s = s3 `composeSubst` s2 `composeSubst` s1
  return (s, applySubst s (ps1 ++ ps2), applySubst s tv)

inferLam :: [Pattern] -> Expr -> InferM (Subst, [Pred], Type)
inferLam [] body = inferExpr body
inferLam (p : ps) body = do
  (s1, env1, t1) <- inferPattern p
  withExtendedEnv env1 $ do
    (s2, ps2, t2) <- inferLam ps (applySubst s1 body)
    let s = s2 `composeSubst` s1
        psAll = applySubst s2 ps2
    return (s, psAll, TArrow (applySubst s t1) t2)

{-}
inferLam :: [Pattern] -> Expr -> InferM (Subst, [Pred], Type)
inferLam [PVar x] body = do
  tv <- freshTypeVar
  let scheme = Forall [] [] tv
  extendEnvWithPattern (PVar x) scheme $ do
    (s1, ps1, tBody) <- inferExpr body
    return (s1, ps1, TArrow (applySubst s1 tv) tBody)
inferLam (PVar x : xs) body = do
  tv <- freshTypeVar
  let scheme = Forall [] [] tv
  extendEnvWithPattern (PVar x) scheme $ do
    (s1, ps1, tBody) <- inferLam xs body
    return (s1, ps1, TArrow (applySubst s1 tv) tBody)
inferLam [] _ =
  throwError $ OtherError "Empty lambda argument list"
inferLam _ _ =
  throwError $ OtherError "Only simple variable patterns (PVar) are supported in lambda"
-}

inferLet :: Pattern -> Expr -> Expr -> InferM (Subst, [Pred], Type)
inferLet pat e1 e2 = do
  myTraceE ("[inferLet] pat = " ++ show pat ++ ", e1 = " ++ show e1 ++ ", e2 = " ++ show e2)
  (s1, ps1, t1) <- inferExpr e1
  env <- getEnv
  (sPat, env', tPat) <- inferPattern pat
  -- (sPat, env', tPat) <- inferPattern (convertPatternFromAPtoTP pat)
  s2 <- unify t1 tPat
  let s12 = composeMany [s2, s1]
      env'' = applySubst s12 env
      envExt = combineEnvs [env', env'']
      sc = generalize envExt (applySubst s12 tPat)
  extendEnvWithPattern pat sc $ do
    (s3, ps2, t2) <- inferExpr (applySubst s12 e2)
    let s = composeMany [s3, s12]
        ps = ps1 ++ ps2
    return (s, ps, t2)

inferIf :: Expr -> Expr -> Expr -> InferM (Subst, [Pred], Type)
inferIf cond eThen eElse = do
  myTraceE ("[inferIf] cond = " ++ show cond ++ ", eThen = " ++ show eThen ++ ", eElse = " ++ show eElse)
  (s1, ps1, tCond) <- inferExpr cond
  sCond <- unify tCond (TCon "Bool")
  let s1' = composeMany [sCond, s1]
  (s2, ps2, tThen) <- inferExpr (applySubst s1' eThen)
  (s3, ps3, tElse) <- inferExpr (applySubst s2 (applySubst s1' eElse))
  s4 <- unify (applySubst s3 tThen) tElse
  let s = composeMany [s4, s3, s2, s1']
      ps = ps1 ++ ps2 ++ ps3
  return (s, ps, applySubst s tThen)

inferAnn :: Expr -> Type -> InferM (Subst, [Pred], Type)
inferAnn e tAnn = do
  myTraceE ("[inferAnn] e = " ++ show e ++ ", tAnn = " ++ show tAnn)
  (s1, ps, tInferred) <- inferExpr e
  s2 <- unify tAnn tInferred
  let s = composeMany [s2, s1]
  return (s, ps, applySubst s tAnn)

inferVar :: Name -> InferM (Subst, [Pred], Type)
inferVar x = do
  TypeEnv env <- getEnv
  case Map.lookup x env of
    Just sigma -> do
      t <- instantiate sigma
      -- let _ = myTraceE ("[Var] Found: " ++ show x ++ " ↦ " ++ show t) ()
      return (emptySubst, [], t)
    Nothing -> throwError $ UnboundVariable x

inferList :: [Expr] -> InferM (Subst, [Pred], Type)
inferList es = do
  myTraceE ("[inferList] es = " ++ show es)
  results <- mapM inferExpr es
  let (substs, predsList, types) = unzip3 results
      s = foldr composeSubst emptySubst substs
      preds = concat predsList
  (s', tElem) <- unifyListElems (map (applySubst s) types)
  let sFinal = s' `composeSubst` s
  return (sFinal, applySubst sFinal preds, TApp (TCon "List") tElem)

inferCase :: Expr -> [CaseAlt] -> InferM (Subst, [Pred], Type)
inferCase e alts = do
  (s1, ps1, tScrutinee) <- inferExpr e
  tv <- freshTypeVar
  (sAlts, psAlts) <- inferCaseAlts (applySubst s1 tScrutinee) alts tv
  let s = sAlts `composeSubst` s1
  return (s, applySubst sAlts (ps1 ++ psAlts), applySubst s tv)

inferCaseAlts :: Type -> [CaseAlt] -> Type -> InferM (Subst, [Pred])
inferCaseAlts _ [] _ = return (emptySubst, [])
inferCaseAlts tScrutinee (alt : alts) tExpected =
  case alt of
    CaseAlt pat expr ->
      inferSimpleAlt tScrutinee pat expr alts tExpected
    CaseAltGuard pat guards ->
      inferGuardedAlt tScrutinee pat guards alts tExpected

inferSimpleAlt :: Type -> Pattern -> Expr -> [CaseAlt] -> Type -> InferM (Subst, [Pred])
inferSimpleAlt tScrutinee pat expr alts tExpected = do
  myTraceE ("[inferSimpleAlt] tScrutinee = " ++ show tScrutinee ++ ", pat = " ++ show pat ++ ", expr = " ++ show expr ++ ", tExpected = " ++ show tExpected)
  (s1, env1, tPat) <- inferPattern pat
  s2 <- unify tScrutinee tPat
  withExtendedEnv env1 $ do
    (s3, ps, tBody) <- inferExpr (applySubst s2 expr)
    s4 <- unify (applySubst s3 tBody) (applySubst s3 tExpected)
    let s = composeMany [s4, s3, s2, s1]
    (sRest, psRest) <-
      inferCaseAlts
        (applySubst s tScrutinee)
        (map (applySubstToAlt s) alts)
        (applySubst s tExpected)
    return (composeMany [sRest, s], ps ++ applySubst s psRest)

inferGuardedAlt :: Type -> Pattern -> [(Expr, Expr)] -> [CaseAlt] -> Type -> InferM (Subst, [Pred])
inferGuardedAlt tScrutinee pat guards alts tExpected = do
  myTraceE
    ( "[inferGuardedAlt] tScrutinee = "
        ++ show tScrutinee
        ++ ", pat = "
        ++ show pat
        ++ ", guards = "
        ++ show guards
        ++ ", tExpected = "
        ++ show tExpected
    )
  (s1, env1, tPat) <- inferPattern pat
  s2 <- unify tScrutinee tPat
  results <- forM guards $ \(cond, body) ->
    withExtendedEnv env1 $ do
      (sc, pc, tc) <- inferExpr (applySubst s2 cond)
      sCond <- unify tc (TCon "Bool")
      (sb, pb, tb) <- inferExpr (applySubst (sCond `composeSubst` sc) body)
      sBody <- unify (applySubst sb tb) (applySubst sb tExpected)
      let s = composeMany [sBody, sb, sCond, sc]
      return (s, pc ++ pb)
  let (subs, predsList) = unzip results
      sGuards = foldr composeSubst emptySubst subs
      preds = concat predsList
  (sRest, psRest) <-
    inferCaseAlts
      (applySubst sGuards tScrutinee)
      (map (applySubstToAlt sGuards) alts)
      (applySubst sGuards tExpected)
  return (composeMany [sRest, sGuards, s2, s1], preds ++ applySubst sGuards psRest)

withExtendedEnv :: TypeEnv -> InferM a -> InferM a
withExtendedEnv env action = do
  oldEnv <- getEnv
  extendEnvRaw env
  result <- action
  putEnv oldEnv
  return result
