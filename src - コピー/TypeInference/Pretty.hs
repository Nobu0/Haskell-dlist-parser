module TypeInference.Pretty (prettyType) where

-- import TypeInference.Subst
-- import TypeInference.Type (Type(..))
-- import qualified TypeInference.Type as TI

import AST.Type (Type(..))
import qualified AST.Type as TI

prettyType :: TI.Type -> String
prettyType ty = case ty of
  TVar v        -> v
  TCon c        -> c
  TUnit         -> "Unit"
  TArrow a b    -> "(" ++ prettyType a ++ " -> " ++ prettyType b ++ ")"
  TList t       -> "[" ++ prettyType t ++ "]"
  TApp a b      -> prettyType a ++ " " ++ prettyType b
  TForall vs t  -> "forall " ++ unwords vs ++ ". " ++ prettyType t
  TTuple ts     -> "(" ++ commaList (map prettyType ts) ++ ")"
  TConstraint cs t ->
      "(" ++ commaList (map show cs) ++ ") => " ++ prettyType t
  where
    commaList = foldr1 (\a b -> a ++ ", " ++ b)
