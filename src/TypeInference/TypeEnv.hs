module TypeInference.TypeEnv
  ( Scheme (..),
    TypeEnv (..),
    emptyEnv,
    primitiveEnv,
    extendEnv,
    lookupEnv,
    generalize,
    instantiate,
    applyEnv,
    freeTypeVars,
    freeTypeVarsScheme,
    freeTypeVarsEnv,
  )
where

import AST.Expr (Name)
import AST.Type (Constraint (..), Type (..))
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace)
import TypeInference.Error (InferError (..))
import TypeInference.Subst (Subst, apply)

-- import Utils.MyTrace -- (myTraceE)

-- 型スキーム：forall a b. t
data Scheme = Forall [Name] Type
  deriving (Show, Eq)

-- 型環境：変数名 → 型スキーム
newtype TypeEnv = TypeEnv (M.Map Name Scheme)
  deriving (Show, Eq)

-- 空の環境（必要に応じて primitiveEnv を使ってもOK）
emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

-- 環境に変数とスキームを追加
extendEnv :: TypeEnv -> Name -> Scheme -> TypeEnv
extendEnv (TypeEnv env) x s = TypeEnv (M.insert x s env)

-- 環境から変数のスキームを取得
lookupEnv :: TypeEnv -> Name -> Maybe Scheme
lookupEnv (TypeEnv env) x = M.lookup x env

{-}
-- 型の自由型変数を集める
freeTypeVars :: Type -> [Name]
freeTypeVars (TVar v) = [v]
freeTypeVars (TCon _) = []
freeTypeVars (TArrow t1 t2) = freeTypeVars t1 ++ freeTypeVars t2
freeTypeVars (TList t) = freeTypeVars t
freeTypeVars (TApp t1 t2) = freeTypeVars t1 ++ freeTypeVars t2
freeTypeVars (TConstraint cs t) = concatMap freeConstraintVars cs ++ freeTypeVars t
freeTypeVars (TForall vs t) = filter (`notElem` vs) (freeTypeVars t)
freeTypeVars (TTuple ts) = concatMap freeTypeVars ts
freeTypeVars TUnit = []
-}

freeTypeVars :: Type -> [Name]
freeTypeVars t = trace (">> freeTypeVars: " ++ show t) $
  case t of
    TVar v -> [v]
    TCon _ -> []
    TArrow t1 t2 -> freeTypeVars t1 ++ freeTypeVars t2
    TList t' -> freeTypeVars t'
    TApp t1 t2 -> freeTypeVars t1 ++ freeTypeVars t2
    TConstraint cs t' -> concatMap freeConstraintVars cs ++ freeTypeVars t'
    TForall vs t' -> filter (`notElem` vs) (freeTypeVars t')
    TTuple ts -> concatMap freeTypeVars ts
    TUnit -> []

-- 制約の中の自由型変数を集める
freeConstraintVars :: Constraint -> [Name]
freeConstraintVars (Constraint _ ts) = concatMap freeTypeVars ts

-- スキームの自由型変数
freeTypeVarsScheme :: Scheme -> [Name]
freeTypeVarsScheme (Forall vars t) = freeTypeVars t \\ vars

-- 環境の自由型変数
freeTypeVarsEnv :: TypeEnv -> [Name]
freeTypeVarsEnv (TypeEnv env) =
  nub (concatMap freeTypeVarsScheme (M.elems env))

-- 一般化：環境に現れない自由型変数を forall で束縛
generalize :: TypeEnv -> Type -> Scheme
generalize env t =
  let vars = freeTypeVars t \\ freeTypeVarsEnv env
   in Forall (nub vars) t

-- 特殊化：forall を外し、新しい型変数に置き換える
instantiate :: Scheme -> Either InferError Type
instantiate (Forall vars t) =
  let s = M.fromList [(v, TVar ("t_" ++ v)) | v <- vars]
   in Right (apply s t)

-- 型変数を新しい名前に置き換える（簡易版）
freshen :: [Name] -> Type -> Type
freshen vars t =
  let s = M.fromList [(v, TVar ("t_" ++ v)) | v <- vars]
   in apply s t

-- 環境全体に置換を適用
applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s (TypeEnv env) =
  TypeEnv (M.map (\(Forall vs t) -> Forall vs (apply s t)) env)

-- 初期のプリミティブ環境（必要に応じて使う）
primitiveEnv :: TypeEnv
primitiveEnv =
  TypeEnv
    ( M.fromList
        [ ("+", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
          ("-", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
          ("*", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
          ("==", Forall ["a"] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))),
          ("True", Forall [] (TCon "Bool")),
          ("False", Forall [] (TCon "Bool"))
        ]
    )
