module Language.TypeSystem.Infer (infer) where

import Language.TypeSystem.Class
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
import Language.TypeSystem.InferM
import Language.TypeSystem.PatternInfer
import Language.TypeSystem.Subst
import Language.TypeSystem.Syntax

-- | 式の型推論（ディスパッチ）
infer :: Expr -> InferM (Subst, [Pred], Type)
infer expr = case expr of
  -- EVar x -> inferVar x
  EInt n -> inferInt n
  EBool b -> inferBool b
  EChar c -> inferChar c
  EString s -> inferString s
  EUnit -> inferUnit

{-}
  EBinOp o l r -> inferBinOp o l r
  ELam ps body -> inferLam ps body
  EApp f x -> inferApp f x
  ELet p e1 e2 -> inferLet p e1 e2
  ELetBlock bs e -> inferLetBlock bs e
  EIf c t e -> inferIf c t e
  ECase e alts -> inferCase e alts
  ETuple es -> inferTuple es
  EList es -> inferList es
  EAnn e t -> inferAnn e t
  ERecord fields -> inferRecord fields
  EFieldAccess e f -> inferFieldAccess e f
  ERecordUpdate e fs -> inferRecordUpdate e fs
  EOpSectionL o e -> inferOpSectionL o e
  EOpSectionR e o -> inferOpSectionR e o
  EWhere e bs -> inferWhere e bs
  ELambdaCase alts -> inferLambdaCase alts
-}

-- | 整数リテラルの型推論（Num a => a）
inferInt :: Int -> InferM (Subst, [Pred], Type)
inferInt _ = do
  tv <- freshTypeVar
  let pred = IsIn "Num" tv
  addPred pred
  return (emptySubst, [pred], tv)

-- | 真偽値リテラルの型推論（Bool）
inferBool :: Bool -> InferM (Subst, [Pred], Type)
inferBool _ = return (emptySubst, [], TCon "Bool")

-- | 文字リテラルの型推論（Char）
inferChar :: Char -> InferM (Subst, [Pred], Type)
inferChar _ = return (emptySubst, [], TCon "Char")

-- | 文字列リテラルの型推論（[Char]）
inferString :: String -> InferM (Subst, [Pred], Type)
inferString _ = return (emptySubst, [], TApp (TCon "List") (TCon "Char"))

-- | 単位値の型推論（Unit）
inferUnit :: InferM (Subst, [Pred], Type)
inferUnit = return (emptySubst, [], TCon "Unit")
