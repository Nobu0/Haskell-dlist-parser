{-# LANGUAGE FlexibleContexts #-}

module Language.TypeSystem.Infer.Program
  ( inferProgram,
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
import Language.TypeSystem.ClassEnv
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.DeclInstance
import Language.TypeSystem.Env
import Language.TypeSystem.EnvInstance
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
-- import Language.TypeSystem.Generalize
import Language.TypeSystem.Infer.Expr
import Language.TypeSystem.Infer.Subst
import Language.TypeSystem.Infer.Unify
import Language.TypeSystem.InferM
import Language.TypeSystem.Utils.MyTrace
import Language.TypeSystem.Pattern
import qualified Language.TypeSystem.Pattern as TP
import Language.TypeSystem.PatternInfer
import Language.TypeSystem.Syntax

{-}
inferProgram :: TypeEnv -> [Decl] -> InferM (Subst, [Pred], TypeEnv)
inferProgram initialEnv decls = do
  -- 1. import の処理（これは IO 層で済ませてから渡される想定でもよい）
  --    ここでは「すでに import 済みの env が initialEnv に入っている」前提でもよい。
  --    あるいは、事前に IO で resolveImport を回して env を構築してから InferM に渡す。

  -- 2. 型シグネチャを先に登録
  let (typeSigDecls, otherDecls) = partitionTypeSigs decls
  envWithSigs <- registerTypeSigs initialEnv typeSigDecls

  -- 3. 依存解析（DeclValue / DeclFunGroup のみ対象）
  let groups = dependencyAnalysis otherDecls

  -- 4. 各グループを順に推論
  (sFinal, predsFinal, envFinal) <-
    foldM (inferGroup envWithSigs) (nullSubst, [], envWithSigs) groups

  -- 5. 結果を返す
  return (sFinal, predsFinal, envFinal)
-}