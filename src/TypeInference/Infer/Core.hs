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
    InferResult,
  )
where

import AST.Decl (Decl (..))
-- import TypeInference.TypeEnv
import AST.Expr (CaseAlt (..), Expr (..), Name, Stmt (..))
import AST.Pattern (Pattern (..))
import AST.Type (Type (..))
import qualified Control.Exception as TypeInference
import Control.Monad (foldM)
import Data.IORef
import Data.List (nub, (\\))
import qualified Data.Map as M
import Debug.Trace (trace, traceIO, traceShowId)
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Error (InferError (..))
-- import TypeInference.SQLInfer
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (UnifyError (..), unify)

-- inferExpr の返り値：型と代入
type InferResult = (Subst, Type)

freshTypeVar :: Either InferError Type
freshTypeVar =
  Right
    ( TVar
        ( "t"
            ++ show
              ( unsafePerformIO
                  ( do
                      n <- readIORef counter
                      writeIORef counter (n + 1)
                      return n
                  )
              )
        )
    )

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

groupDecls :: [Decl] -> M.Map Name [Decl]
groupDecls decls =
  M.fromListWith (++) [(name, [d]) | d@(DeclFun name _ _ _ _) <- decls]

--  M.fromListWith (++) [(name, [d]) | d@(DeclFun name _ _) <- decls]

unifyMany :: [Type] -> Either InferError Subst
unifyMany [] = Right emptySubst
unifyMany (t : ts) =
  foldM
    ( \sacc t' ->
        case unify (apply sacc t) (apply sacc t') of
          Left _ -> Left (InferMismatch (apply sacc t) (apply sacc t'))
          Right s -> Right (composeSubst s sacc)
    )
    emptySubst
    ts
