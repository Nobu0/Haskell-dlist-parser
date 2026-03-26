module TypeInference.TypeEnv
  ( Scheme (..),
    TypeEnv (..),
    emptyEnv,
    -- mergeEnvs,
    primitiveEnv,
    extendEnv,
    lookupEnv,
    generalize,
    instantiate,
    applyEnv,
    freeTypeVars,
    freeTypeVarsScheme,
    freeTypeVarsEnv,
    fromList,
  )
where

-- mport AST.Expr (Name)
import Data.List (nub, (\\))
import qualified Data.Map as M
import qualified Data.Map as Map
import Debug.Trace (trace)
import TypeInference.Error (InferError (..))
import TypeInference.Subst (Subst, apply)
import TypeInference.Type

-- type Name = String

-- 型スキーム：forall a b. t
data Scheme = Forall [Name] Type
  deriving (Show, Eq)

-- 型環境：変数名 → 型スキーム
newtype TypeEnv = TypeEnv (M.Map Name Scheme)
  deriving (Show, Eq)

fromList :: [(String, Scheme)] -> TypeEnv
fromList = TypeEnv . Map.fromList

-- 空の環境
emptyEnv :: TypeEnv
emptyEnv = TypeEnv M.empty

-- 環境に変数とスキームを追加
extendEnv :: TypeEnv -> Name -> Scheme -> TypeEnv
extendEnv (TypeEnv env) x s = TypeEnv (M.insert x s env)

-- 環境から変数のスキームを取得
-- lookupEnv :: TypeEnv -> Name -> Maybe Scheme
-- lookupEnv (TypeEnv env) x = M.lookup x env
lookupEnv :: TypeEnv -> Name -> Maybe Scheme
lookupEnv (TypeEnv m) name = Map.lookup name m

-- 型の自由型変数を集める
freeTypeVars :: Type -> [Name]
freeTypeVars t = case t of
  TVar v -> [v]
  TCon _ -> []
  TArrow t1 t2 -> freeTypeVars t1 ++ freeTypeVars t2
  TList t' -> freeTypeVars t'
  TApp t1 t2 -> freeTypeVars t1 ++ freeTypeVars t2
  TConstraint cs t' -> concatMap freeConstraintVars cs ++ freeTypeVars t'
  TForall vs t' -> filter (`notElem` vs) (freeTypeVars t')
  TTuple ts -> concatMap freeTypeVars ts
  TUnit -> []
  TRecord fields -> concatMap freeTypeVars (M.elems fields)

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
          (">", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool")))),
          ("$", Forall ["a", "b"] (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "b")))),
          ("print", Forall ["a"] (TArrow (TVar "a") (TCon "Unit"))),
          ("True", Forall [] (TCon "Bool")),
          ("False", Forall [] (TCon "Bool")),
          ("x", Forall [] (TCon "Int")),
          ("a", Forall [] (TCon "Int")),
          ("b", Forall [] (TCon "Int")),
          ("c", Forall [] (TCon "Int")),
          -- ("y", Forall [] (TCon "Int")),
          ("z", Forall [] (TCon "Int")),
          ("v", Forall [] (TApp (TCon "Maybe") (TCon "Int"))),
          ("cond", Forall [] (TCon "Bool")),
          ("cond1", Forall [] (TCon "Bool")),
          ("cond2", Forall [] (TCon "Bool")),
          ("xs", Forall [] (TList (TCon "Int"))),
          ("ys", Forall [] (TList (TCon "Int"))),
          ("zs", Forall [] (TList (TCon "Int"))),
          ("r", Forall [] rType),
          ("r2", Forall [] (TArrow (TRecord (Map.fromList [("x", TCon "Int"), ("y", TCon "Int")])) (TCon "Bool"))),
          ("r3", Forall [] (TArrow (TRecord (Map.fromList [("f", TArrow (TCon "Int") (TCon "Int"))])) (TCon "Bool"))),
          ("f", Forall [] (TArrow (TCon "Int") (TCon "Bool"))),
          ("fi", Forall [] (TArrow (TCon "Int") (TCon "Int"))),
          ("g", Forall [] (TArrow (TCon "Bool") (TCon "Int"))),
          ("ft", Forall [] (TArrow (TCon "Int") (TTuple [TCon "Int", TCon "Bool"])))
        ]
    )

-- 型定義
rType :: Type
rType = TArrow (TRecord (Map.fromList [("x", TCon "Int")])) (TCon "Bool")
