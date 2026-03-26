module TypeInference.Infer.Core
  ( counter,
    builtinPatternEnv,
    builtinOps,
    builtinEnv,
    generalizeInfer,
    groupDecls,
    unifyMany,
    mergeEnvs,
    freshTypeVar,
    freshType,
    InferResult,
    InferM,
    lift,
    runInfer,
  )
where

import AST.Decl (Decl (..), FunClause (..))
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
import AST.Pattern (Pattern (..))
import qualified AST.Type as AST
import qualified Control.Exception as TypeInference
import Control.Monad (foldM)
import Control.Monad.State
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Error (InferError (..))
import TypeInference.Subst
import qualified TypeInference.Type as TI
import TypeInference.TypeEnv
import TypeInference.Unify (UnifyError (..), unify, unifyMany)

-- 型推論の返り値：代入と型
type InferResult = (Subst, TI.Type)

type InferM a = StateT Int (Either InferError) a

runInfer :: InferM a -> Either InferError a
runInfer m = evalStateT m 0

freshType :: InferM TI.Type
freshType = freshTypeVar

freshTypeVar :: InferM TI.Type
freshTypeVar = do
  n <- get
  put (n + 1)
  return (TI.TVar ("t" ++ show n))

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
{-# NOINLINE counter #-}

mergeEnvs :: TypeEnv -> TypeEnv -> TypeEnv
mergeEnvs (TypeEnv e1) (TypeEnv e2) =
  TypeEnv (M.union e1 e2)

builtinPatternEnv :: TypeEnv
builtinPatternEnv =
  TypeEnv
    ( M.fromList
        [ ("Just", Forall ["a"] (TI.TArrow (TI.TVar "a") (TI.TApp (TI.TCon "Maybe") (TI.TVar "a")))),
          ("Nothing", Forall ["a"] (TI.TApp (TI.TCon "Maybe") (TI.TVar "a"))),
          (":", Forall ["a"] (TI.TArrow (TI.TVar "a") (TI.TArrow (TI.TList (TI.TVar "a")) (TI.TList (TI.TVar "a"))))),
          ("[]", Forall ["a"] (TI.TList (TI.TVar "a")))
        ]
    )

builtinOps :: [(String, Scheme)]
builtinOps =
  [ ("++", Forall [] (TI.TArrow (TI.TCon "String") (TI.TArrow (TI.TCon "String") (TI.TCon "String")))),
    ("+", Forall [] (TI.TArrow (TI.TCon "Int") (TI.TArrow (TI.TCon "Int") (TI.TCon "Int")))),
    ("-", Forall [] (TI.TArrow (TI.TCon "Int") (TI.TArrow (TI.TCon "Int") (TI.TCon "Int")))),
    ("*", Forall [] (TI.TArrow (TI.TCon "Int") (TI.TArrow (TI.TCon "Int") (TI.TCon "Int")))),
    ("/", Forall [] (TI.TArrow (TI.TCon "Int") (TI.TArrow (TI.TCon "Int") (TI.TCon "Int"))))
  ]

builtinEnv :: TypeEnv
builtinEnv = TypeEnv (M.fromList builtinOps)

generalizeInfer :: TypeEnv -> TI.Type -> Scheme
generalizeInfer env t =
  let vars = nub (freeTypeVars t \\ freeTypeVarsEnv env)
   in Forall vars t

groupDecls :: [Decl] -> M.Map Name ([FunClause], Maybe AST.Type)
groupDecls decls = foldr go M.empty decls
  where
    go (DeclFunGroup name clauses) acc =
      M.insertWith
        (\(newClauses, _) (oldClauses, oldTy) -> (newClauses ++ oldClauses, oldTy))
        name
        (clauses, Nothing)
        acc
    go (DeclTypeSig name ty) acc =
      M.insertWith
        (\_ (oldClauses, _) -> (oldClauses, Just ty))
        name
        ([], Just ty)
        acc
    go _ acc = acc -- 他のDeclは無視（必要なら拡張）

{-}
unifyMany :: [TI.Type] -> Either InferError Subst
unifyMany [] = Right emptySubst
unifyMany (t : ts) = foldM (unifyStep t) emptySubst ts
-}

unifyStep :: TI.Type -> Subst -> TI.Type -> Either InferError Subst
unifyStep t sacc t' =
  case unify (apply sacc t) (apply sacc t') of
    Left _ -> Left (InferMismatch (apply sacc t) (apply sacc t'))
    Right s -> Right (composeSubst s sacc)
