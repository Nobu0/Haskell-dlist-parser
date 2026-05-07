module Language.TypeSystem.DeclInstance (generalize) where

import Control.Monad (foldM, forM)
import Control.Monad.Combinators (empty)
import Control.Monad.Except (throwError)
import Data.IntMap (null)
import Data.IntMap.Lazy (empty)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.TypeSystem.BaseType
-- import Language.TypeSystem.Class

-- import Language.TypeSystem.Generalize

import Language.TypeSystem.ClassDef (SchemeLike (generalize))
import Language.TypeSystem.ClassDef hiding (generalize)
import Language.TypeSystem.ClassEnv
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.Decl (Decl (..))
import qualified Language.TypeSystem.Decl as D
import Language.TypeSystem.DeclDef
import Language.TypeSystem.Env
import Language.TypeSystem.EnvInstance
import Language.TypeSystem.Error
import Language.TypeSystem.Error (InferError (..))
import Language.TypeSystem.Expr
import Language.TypeSystem.Infer.Expr
import Language.TypeSystem.InferM
import Language.TypeSystem.Utils.MyTrace
import Language.TypeSystem.Pattern
import qualified Language.TypeSystem.Pattern as TP
import Language.TypeSystem.PatternInfer
import Language.TypeSystem.Infer.Subst
import Language.TypeSystem.Syntax
import Language.TypeSystem.Infer.Unify

instance HasType Expr where
  infer expr = do
    (_, _, t) <- inferExpr expr
    return t

instance HasType D.Decl where
  infer (D.DeclValue _ expr) = infer expr
  -- infer _ = throwError "Unsupported Decl form for inference"
  infer _ = throwError (OtherError "Unsupported Decl form for inference")

instance Dependency Decl where
  freeVars (D.DeclValue _ expr) = freeVars expr

instance Dependency Expr where
  freeVars (EVar x) = Set.singleton x
  freeVars (EInt _) = Set.empty
  freeVars (EBool _) = Set.empty
  freeVars (EChar _) = Set.empty
  freeVars (EString _) = Set.empty
  freeVars EUnit = Set.empty
  freeVars (EBinOp _ e1 e2) =
    freeVars e1 `Set.union` freeVars e2
  freeVars (ELam pats body) =
    freeVars body `Set.difference` Set.unions (map boundVars pats)
  freeVars (EApp e1 e2) =
    freeVars e1 `Set.union` freeVars e2
  freeVars (ELet pat e1 e2) =
    (freeVars e1 `Set.union` freeVars e2)
      `Set.difference` boundVars pat
  freeVars (ELetBlock binds body) =
    let fvBinds = Set.unions [freeVars e | (_, e) <- binds]
        fvBody = freeVars body
        bound = Set.unions [boundVars p | (p, _) <- binds]
     in (fvBinds `Set.union` fvBody) `Set.difference` bound
  freeVars (EIf c t f) =
    freeVars c `Set.union` freeVars t `Set.union` freeVars f
  freeVars (ECase e alts) =
    freeVars e `Set.union` Set.unions (map freeVars alts)
  freeVars (ETuple es) =
    Set.unions (map freeVars es)
  freeVars (EList es) =
    Set.unions (map freeVars es)
  freeVars (EAnn e _) =
    freeVars e
  freeVars (ERecord fields) =
    Set.unions (map freeVars (Map.elems fields))
  freeVars (EFieldAccess e _) =
    freeVars e
  freeVars (ERecordUpdate e updates) =
    freeVars e `Set.union` Set.unions (map (freeVars . snd) updates)
  freeVars (EOpSectionL _ e) =
    freeVars e
  freeVars (EOpSectionR e _) =
    freeVars e
  freeVars (EWhere e binds) =
    let fvBinds = Set.unions [freeVars expr | (_, expr) <- binds]
        fvBody = freeVars e
        bound = Set.unions [boundVars pat | (pat, _) <- binds]
     in (fvBinds `Set.union` fvBody) `Set.difference` bound
  freeVars (ELambdaCase alts) =
    Set.unions (map freeVars alts)

instance Dependency CaseAlt where
  freeVars (CaseAlt pat expr) =
    freeVars expr `Set.difference` boundVars pat
  freeVars (CaseAltGuard pat guards) =
    let fvGuards = Set.unions [freeVars c `Set.union` freeVars e | (c, e) <- guards]
     in fvGuards `Set.difference` boundVars pat

boundVars :: Pattern -> Set.Set Name
boundVars (PVar x) = Set.singleton x
boundVars PWildcard = Set.empty
boundVars (PTuple ps) = Set.unions (map boundVars ps)
boundVars (PCons _ p) = boundVars p
