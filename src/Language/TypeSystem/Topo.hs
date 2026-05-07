module Language.TypeSystem.Topo where

import Control.Monad (foldM, forM)
import Control.Monad.Combinators (empty)
import Control.Monad.Except (throwError)
-- import System.Environment (getEnv)

import Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.BaseType
import Language.TypeSystem.ClassDef hiding (generalize)
import Language.TypeSystem.Decl (Decl (..))
import qualified Language.TypeSystem.Decl as D
import Language.TypeSystem.DeclDef
import Language.TypeSystem.DeclInstance
import Language.TypeSystem.Error (InferError (..))
import Language.TypeSystem.Expr
import Language.TypeSystem.Infer.Expr
import Language.TypeSystem.InferM
import Language.TypeSystem.Pattern
import Language.TypeSystem.Syntax

definedNames :: Decl -> [Name]
definedNames (DeclValue (PVar n) _) = [n]
definedNames (DeclFunGroup n _) = [n]
definedNames (DeclTypeSig n _) = [n]
definedNames _ = []

usedNames :: Decl -> Set.Set Name
usedNames decl = freeVars decl

buildGraph :: [Decl] -> Map.Map Name (Set.Set Name)
buildGraph decls =
  Map.fromList
    [ (n, usedNames decl)
      | decl <- decls,
        n <- definedNames decl
    ]

topoSortDecls :: [Decl] -> [Decl]
topoSortDecls decls =
  let graph = buildGraph decls
      order = topoSort graph
   in reorderDecls order decls

topoSort :: Map.Map Name (Set.Set Name) -> [Name]
topoSort graph = go [] (Map.keys graph)
  where
    go sorted [] = reverse sorted
    go sorted remaining =
      let (noDeps, rest) =
            partition (\n -> Set.null (graph Map.! n)) remaining
       in if null noDeps
            then error "Cyclic dependency detected"
            else
              go
                (noDeps ++ sorted)
                [ n | n <- rest, not (any (`Set.member` (graph Map.! n)) noDeps)
                ]

reorderDecls :: [Name] -> [Decl] -> [Decl]
reorderDecls order decls =
  concat
    [ [decl | decl <- decls, n <- definedNames decl, n == name]
      | name <- order
    ]
