module TypeInference.TypeEnv
  ( Scheme (..),
    TypeEnv (..),
    emptyEnv,
    extendEnv,
    lookupEnv,
    generalize,
    instantiate,
    applyEnv,
    freeTypeVarsScheme,
    freeTypeVarsEnv,
    freeTypeVars,
  )
where

import AST.Expr (Name)
import AST.Type
import AST.Type (Type (..))
import Data.List (nub, (\\))
import qualified Data.Map as M
import TypeInference.Subst

-- 型スキーム：forall a b. t
data Scheme = Forall [String] Type
  deriving (Show, Eq)

-- 型環境：変数名 → 型スキーム
newtype TypeEnv = TypeEnv (M.Map String Scheme)
  deriving (Show, Eq)

{-}
emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty
-}

emptyEnv :: TypeEnv
emptyEnv = primitiveEnv

extendEnv :: TypeEnv -> String -> Scheme -> TypeEnv
extendEnv (TypeEnv env) x s = TypeEnv (M.insert x s env)

lookupEnv :: TypeEnv -> String -> Maybe Scheme
lookupEnv (TypeEnv env) x = M.lookup x env

freeTypeVarsEnv :: TypeEnv -> [Name]
freeTypeVarsEnv (TypeEnv env) =
  nub (concatMap freeTypeVarsScheme (M.elems env))

freeTypeVarsScheme :: Scheme -> [Name]
freeTypeVarsScheme (Forall vars t) =
  freeTypeVars t \\ vars

-- 型の自由変数を集める
freeTypeVars :: Type -> [String]
freeTypeVars t = case t of
  TVar v -> [v]
  TCon _ -> []
  TArrow t1 t2 -> freeTypeVars t1 ++ freeTypeVars t2
  TList t1 -> freeTypeVars t1
  TApp t1 t2 -> freeTypeVars t1 ++ freeTypeVars t2
  TConstraint cs t1 ->
    concatMap freeConstraintVars cs ++ freeTypeVars t1
  TForall vs t1 ->
    filter (`notElem` vs) (freeTypeVars t1)

freeConstraintVars :: Constraint -> [String]
freeConstraintVars (Constraint _ ts) =
  concatMap freeTypeVars ts

-- 環境の自由型変数
freeEnvVars :: TypeEnv -> [String]
freeEnvVars (TypeEnv env) =
  concatMap (\(Forall _ t) -> freeTypeVars t) (M.elems env)

-- 一般化：env の自由変数を除いた型変数を forall で束縛する
generalize :: TypeEnv -> Type -> Scheme
generalize env t =
  let envVars = freeEnvVars env
      typeVars = freeTypeVars t
      vars = filter (`notElem` envVars) typeVars
   in Forall vars t

-- 特殊化：forall を外し、新しい型変数に置き換える
instantiate :: Scheme -> Type
instantiate (Forall vars t) =
  let fresh v = TVar (v ++ "'") -- 簡易的な新しい型変数
      s = M.fromList [(v, fresh v) | v <- vars]
   in apply s t

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s (TypeEnv env) =
  TypeEnv (M.map (\(Forall vs t) -> Forall vs (apply s t)) env)

primitiveEnv :: TypeEnv
primitiveEnv =
  TypeEnv
    ( M.fromList
        [ ("+", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
          ("-", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
          ("*", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
          ("==", Forall ["a"] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool"))))
        ]
    )
