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
-- import TypeInference.TypeEnv
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
import AST.Pattern (Pattern (..))
import AST.Type (Type (..))
import qualified Control.Exception as TypeInference
import Control.Monad (foldM)
-- import TypeInference.SQLInfer

import Control.Monad.State
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Error (InferError (..))
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (UnifyError (..), unify)

-- inferExpr の返り値：型と代入
type InferResult = (Subst, Type)

-- type Infer a = State Int a

type InferM a = StateT Int (Either InferError) a

runInfer :: InferM a -> Either InferError a
runInfer m = evalStateT m 0

freshType :: InferM Type
freshType = freshTypeVar

freshTypeVar :: InferM Type
freshTypeVar = do
  n <- get
  put (n + 1)
  return (TVar ("t" ++ show n))

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
        [ ("Just", Forall ["a"] (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "a")))),
          ("Nothing", Forall ["a"] (TApp (TCon "Maybe") (TVar "a"))),
          (":", Forall ["a"] (TArrow (TVar "a") (TArrow (TList (TVar "a")) (TList (TVar "a"))))),
          ("[]", Forall ["a"] (TList (TVar "a")))
        ]
    )

builtinOps :: [(String, Scheme)]
builtinOps =
  [ ("++", Forall [] (TArrow (TCon "String") (TArrow (TCon "String") (TCon "String")))),
    ("+", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
    ("-", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
    ("*", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int")))),
    ("/", Forall [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))))
  ]

builtinEnv :: TypeEnv
builtinEnv = TypeEnv (M.fromList builtinOps)

generalizeInfer :: TypeEnv -> Type -> Scheme
generalizeInfer env t =
  let vars = nub (freeTypeVars t \\ freeTypeVarsEnv env)
   in Forall vars t

-- groupDecls :: [Decl] -> M.Map Name [Decl]
-- groupDecls decls =
--  M.fromListWith (++) [(name, [d]) | d@(DeclFunGroup name _) <- decls]
{-}
groupDecls :: [Decl] -> M.Map Name [FunClause]
groupDecls decls =
  M.fromListWith (++) [(name, clauses) | DeclFunGroup name clauses <- decls]
-}

groupDecls :: [Decl] -> M.Map Name ([FunClause], Maybe Type)
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

--  M.fromListWith (++) [(name, [d]) | d@(DeclFun name _ _) <- decls]
{-}
unifyMany :: [Type] -> Either InferError Subst
unifyMany [] = Right emptySubst
unifyMany (t : ts) =
  foldM
    ( ( \sacc t' ->
          case unify (apply sacc t) (apply sacc t') of
            Left _ -> Left (InferMismatch (apply sacc t) (apply sacc t'))
            Right s -> Right (composeSubst s sacc)
      )
        emptySubst
        ts
    )
-}
unifyMany :: [Type] -> Either InferError Subst
unifyMany [] = Right emptySubst
unifyMany (t : ts) = foldM (unifyStep t) emptySubst ts

unifyStep :: Type -> Subst -> Type -> Either InferError Subst
unifyStep t sacc t' =
  case unify (apply sacc t) (apply sacc t') of
    Left _ -> Left (InferMismatch (apply sacc t) (apply sacc t'))
    Right s -> Right (composeSubst s sacc)
