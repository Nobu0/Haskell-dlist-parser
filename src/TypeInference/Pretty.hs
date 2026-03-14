module TypeInference.Pretty (prettyType, prettyPrintBinOp, Pretty (..)) where

-- 必要に応じてインポート

import AST.BinOp (BinOp (..))
import qualified AST.BinOp as B
import AST.Type (Type (..))
import qualified AST.Type as TI
import Prettyprinter
import Prettyprinter (Pretty (..), parens, pretty, (<+>))
import Prettyprinter.Render.Terminal (putDoc)

prettyPrintBinOp :: B.BinOp -> String
prettyPrintBinOp op =
  case op of
    B.BinOpAdd -> "+"
    B.BinOpSub -> "-"
    B.BinOpMul -> "*"
    B.BinOpDiv -> "/"
    B.BinOpEq -> "=="
    B.BinOpNeq -> "/="
    B.BinOpLt -> "<"
    B.BinOpGt -> ">"
    B.BinOpLe -> "<="
    B.BinOpGe -> ">="
    B.BinOpAnd -> "&&"
    B.BinOpOr -> "||"
    B.BinOpConcat -> "++"
    B.BinOpCons -> ":"
    B.BinOpCompose -> "."
    B.BinOpThen -> ">>"
    B.BinOpBind -> ">>="
    B.BinOpAlt -> "<|>"
    B.BinOpFmap -> "<$>"
    B.BinOpApp -> "<*>"

instance Pretty Type where
  pretty ty =
    case ty of
      TVar v -> pretty v
      TCon c -> pretty c
      TUnit -> pretty "Unit"
      TArrow a b ->
        parens (pretty a <+> pretty "->" <+> pretty b)
      TList t ->
        brackets (pretty t)
      TApp a b ->
        pretty a <+> pretty b
      TForall vs t ->
        pretty "forall" <+> hsep (map pretty vs) <> pretty "." <+> pretty t
      TTuple ts ->
        tupled (map pretty ts)
      TBinOp op t1 t2 ->
        parens (pretty t1 <+> pretty (prettyPrintBinOp op) <+> pretty t2)
      TConstraint cs t ->
        parens (hsep (punctuate (pretty ",") (map (pretty . show) cs))) <+> pretty "=>" <+> pretty t

prettyType :: TI.Type -> String
prettyType ty = do
  case ty of
    TVar v -> v
    TCon c -> c
    TUnit -> "Unit"
    TArrow a b -> "(" ++ prettyType a ++ " -> " ++ prettyType b ++ ")"
    TList t -> "[" ++ prettyType t ++ "]"
    TApp a b -> prettyType a ++ " " ++ prettyType b
    TForall vs t -> "forall " ++ unwords vs ++ ". " ++ prettyType t
    TTuple ts -> "(" ++ commaList (map prettyType ts) ++ ")"
    TBinOp op t1 t2 ->
      "(" ++ prettyType t1 ++ " " ++ prettyPrintBinOp op ++ " " ++ prettyType t2 ++ ")"
    TConstraint cs t ->
      "(" ++ commaList (map show cs) ++ ") => " ++ prettyType t
  where
    commaList = foldr1 (\a b -> a ++ ", " ++ b)
