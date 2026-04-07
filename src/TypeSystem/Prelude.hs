module Language.TypeSystem.Prelude (preludeDataEnv, initialEnv) where

import qualified Data.Map as Map
-- TCon, TVar, TApp, etc.
-- Forall, etc.
import Language.TypeSystem.BaseType -- Name type
import Language.TypeSystem.Env
import Language.TypeSystem.Syntax

intBinOp :: Type
intBinOp = TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Int"))

numBinOp :: Type
numBinOp = TArrow (TVar "a") (TArrow (TVar "a") (TVar "a"))

-- 初期の型環境
initialEnv :: TypeEnv
initialEnv =
  TypeEnv $
    Map.fromList
      [ ("+", Forall ["a"] [IsIn "Num" (TVar "a")] (numBinOp)),
        ("-", Forall ["a"] [IsIn "Num" (TVar "a")] (numBinOp)),
        ("*", Forall ["a"] [IsIn "Num" (TVar "a")] (numBinOp)),
        (">", Forall ["a"] [IsIn "Ord" (TVar "a")] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))),
        ("<", Forall ["a"] [IsIn "Ord" (TVar "a")] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))),
        ("==", Forall ["a"] [IsIn "Eq" (TVar "a")] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))),
        ("&&", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool")))),
        ("||", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool")))),
        ("not", Forall [] [] (TArrow (TCon "Bool") (TCon "Bool"))),
        ("$", Forall ["a", "b"] [] (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "b")))),
        (".", Forall ["a", "b", "c"] [] (TArrow (TArrow (TVar "b") (TVar "c")) (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "c"))))),
        ("++", Forall ["a"] [] (TArrow (TList (TVar "a")) (TArrow (TList (TVar "a")) (TList (TVar "a")))))
      ]

initialEnvx :: TypeEnv
initialEnvx =
  TypeEnv $
    Map.fromList
      [ ("+", Forall [] [] (intBinOp)),
        ("-", Forall [] [] (intBinOp)),
        ("*", Forall [] [] (intBinOp)),
        ("div", Forall [] [] (intBinOp)),
        ("mod", Forall [] [] (intBinOp)),
        ("==", Forall ["a"] [] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))),
        ("/=", Forall ["a"] [] (TArrow (TVar "a") (TArrow (TVar "a") (TCon "Bool")))),
        ("<", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool")))),
        ("<=", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool")))),
        (">", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool")))),
        (">=", Forall [] [] (TArrow (TCon "Int") (TArrow (TCon "Int") (TCon "Bool")))),
        ("&&", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool")))),
        ("||", Forall [] [] (TArrow (TCon "Bool") (TArrow (TCon "Bool") (TCon "Bool")))),
        ("not", Forall [] [] (TArrow (TCon "Bool") (TCon "Bool"))),
        ("$", Forall ["a", "b"] [] (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "b")))),
        (".", Forall ["a", "b", "c"] [] (TArrow (TArrow (TVar "b") (TVar "c")) (TArrow (TArrow (TVar "a") (TVar "b")) (TArrow (TVar "a") (TVar "c"))))),
        ("++", Forall ["a"] [] (TArrow (TList (TVar "a")) (TArrow (TList (TVar "a")) (TList (TVar "a")))))
      ]

{-}
-- 初期状態
initialInferState :: InferState
initialInferState = InferState
  { count = 0
  , env = preludeEnv  -- ✅ ここ！
  , dataEnv = preludeDataEnv
  }
-}
preludeDataEnv :: Map.Map Name Scheme
preludeDataEnv =
  Map.fromList
    [ ("True", Forall [] [] (TCon "Bool")),
      ("False", Forall [] [] (TCon "Bool")),
      ("Nothing", Forall ["a"] [] (TApp (TCon "Maybe") (TVar "a"))),
      ("Just", Forall ["a"] [] (TArrow (TVar "a") (TApp (TCon "Maybe") (TVar "a")))),
      ("[]", Forall ["a"] [] (TList (TVar "a"))),
      (":", Forall ["a"] [] (TArrow (TVar "a") (TArrow (TList (TVar "a")) (TList (TVar "a"))))),
      ("()", Forall [] [] (TCon "Unit")),
      ("(,)", Forall ["a", "b"] [] (TArrow (TVar "a") (TArrow (TVar "b") (TTuple [TVar "a", TVar "b"])))),
      ("print", Forall ["a"] [] (TArrow (TVar "a") (TCon "Unit"))),
      ("reverse", Forall ["a"] [] (TArrow (TList (TVar "a")) (TList (TVar "a")))),
      ("span", Forall ["a"] [] (TArrow (TArrow (TVar "a") (TCon "Bool")) (TArrow (TList (TVar "a")) (TTuple [TList (TVar "a"), TList (TVar "a")])))),
      ("drop", Forall ["a"] [] (TArrow (TCon "Int") (TArrow (TList (TVar "a")) (TList (TVar "a")))))
    ]
