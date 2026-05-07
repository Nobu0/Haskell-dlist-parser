module Language.TypeSystem.Pretty
  ( ppType,
    printInferResult,
  )
where

import qualified Data.Map as Map
import Language.TypeSystem.BaseType
import Language.TypeSystem.Class
import Language.TypeSystem.Env
import Language.TypeSystem.Infer
import Language.TypeSystem.Infer.Subst
import Language.TypeSystem.Syntax
import Language.TypeSystem.Utils.Utils

showField :: (Name, Type) -> String
showField (name, ty) = name ++ " : " ++ ppType ty

ppType :: Type -> String
ppType = go False
  where
    go _ (TVar n) = n
    go _ (TCon c) = c
    go p (TArrow t1 t2) =
      let left = go True t1
          right = go False t2
          s = left ++ " -> " ++ right
       in if p then "(" ++ s ++ ")" else s
    go _ (TList t) = "[" ++ go False t ++ "]"
    go _ (TTuple ts) = "(" ++ commaSep (map (go False) ts) ++ ")"
    go _ (TApp t1 t2) = go True t1 ++ " " ++ go True t2
    go _ (TRecord fields Nothing) =
      "{" ++ commaSep (map showField (Map.toList fields)) ++ "}"
    go _ (TRecord fields (Just row)) =
      "{" ++ commaSep (map showField (Map.toList fields) ++ ["|" ++ go False row]) ++ "}"
    {-}
    go _ (TRecord fields) =
      let showField (name, ty) = name ++ " = " ++ go False ty
       in "{" ++ commaSep (map showField (Map.toList fields)) ++ "}"
        go _ (TForall vars preds t) =
          let varsStr = unwords vars
              predsStr = if null preds then "" else show preds ++ " => "
           in "forall " ++ varsStr ++ ". " ++ predsStr ++ go False t
    -}
    commaSep = foldr1 (\a b -> a ++ ", " ++ b)

printInferResult :: (Subst, [Pred], Type) -> IO ()
printInferResult (s, preds, ty) = do
  putStrLn $ "Type: " ++ ppType (applySubst s ty)
  putStrLn $ "Subst: " ++ show s
  putStrLn $ "Preds: " ++ show (simplifyPreds (applySubst s preds))
