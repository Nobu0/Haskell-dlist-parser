module Language.TypeSystem.InferDecl
  ( inferDecl,
  )
where

import Control.Monad (foldM, forM)
import Control.Monad.Combinators (empty)
import Control.Monad.Except (throwError)
import Data.IntMap (null)
import Data.IntMap.Lazy (empty)
import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.Class
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.Env
import Language.TypeSystem.EnvInstance
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
import Language.TypeSystem.Generalize
import Language.TypeSystem.InferExpr
import Language.TypeSystem.InferM
import Language.TypeSystem.MyTrace
import Language.TypeSystem.Pattern
import qualified Language.TypeSystem.Pattern as TP
import Language.TypeSystem.PatternInfer
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax
import Language.TypeSystem.Unify

-- メインディスパッチ関数
inferDecl :: Decl -> InferM (Subst, [Pred], TypeEnv)
inferDecl decl = case decl of
  -- DeclValue pat expr -> inferValueDecl pat expr
  DeclFunGroup name clauses -> inferFunGroupDecl name clauses
  DeclTypeSig _ _ ->
    -- 型注釈単体では型環境に影響しない（後で結合）
    return (nullSubst, [], emptyEnv)
  -- 今は未対応の宣言はエラーにする
  other ->
    throwError $ OtherError $ "inferDecl: unsupported declaration: " ++ show other

inferFunGroupDecl :: Name -> [FunClause] -> InferM (Subst, [Pred], TypeEnv)
inferFunGroupDecl name clauses = do
  -- 各節をラムダ式に変換
  let exprs = map clauseToExpr clauses
  -- 各節を推論して型を得る
  results <- mapM inferExpr exprs
  let substs = map (\(s, _, _) -> s) results
      preds = concatMap (\(_, ps, _) -> ps) results
      types = map (\(_, _, t) -> t) results
  -- 全ての節の型を unify
  t <- freshTypeVar
  -- sUnify <- foldM unify t types
  sUnify <-
    foldM
      ( \s ty -> do
          s' <- unify (applySubst s ty) t
          return (s' `composeSubst` s)
      )
      nullSubst
      types
  let finalSubst = foldr composeSubst sUnify substs
      finalType = applySubst finalSubst t
      scheme = generalize emptyEnv finalType
  return (finalSubst, applySubst finalSubst preds, singletonEnv name scheme)

clauseToExpr (FunClause pats Nothing (Just body) Nothing) =
  foldr ELam body [pats]
clauseToExpr (FunClause _ _ _ (Just _)) =
  error "where declarations not yet supported"

{-}
clauseToExpr (FunClause pats Nothing (Just body) whereDecls) =
  let withWhere = maybe body (EWhere body) whereDecls
   in foldr ELam withWhere pats
clauseToExpr (FunClause _ (Just _) _ _) =
  error "Guarded clauses not yet supported"

clauseToExpr :: FunClause -> Expr
clauseToExpr (FunClause pats guards body whereDecls) =
  let core = case guards of
        Just gs -> EGuarded gs
        Nothing -> maybe (error "Missing body in clause") id body
      withWhere = maybe core (EWhere core) whereDecls
   in foldr ELam withWhere pats
-}
