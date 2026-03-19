module Main where

import AST.Decl
import AST.Expr
import AST.Pattern
import AST.Type
import Debug.Trace
import TypeInference.Infer
import TypeInference.TypeEnv
import Utils.MyTrace -- (setTrace)

main :: IO ()
main = do
  setTrace True
  let decls =
        [ DeclTypeSig "h" (TArrow (TList (TCon "Int")) (TCon "Int")),
          DeclFunGroup
            "h"
            [ FunClause [PList []] Nothing (Just (EInt 0)) Nothing,
              FunClause
                [PCons (PVar "x") (PVar "xs")]
                Nothing
                (Just (EApp (EApp (EVar "+") (EVar "x")) (EApp (EVar "h") (EVar "xs"))))
                Nothing
            ]
        ]

  myTraceIOM ("<< main: decls " ++ show decls)

  -- trace ("Inferring: " ++ show expr) `seq` inferExpr env expr
  case inferProgram emptyEnv decls of
    Left err -> putStrLn ("Error: " ++ show err)
    Right env -> putStrLn ("Success: " ++ show env)

{-}
  let decls =
        [ DeclFun "f" [PConstr "Just" [PVar "x"]] (EVar "x"),
          DeclFun "g" [PCons (PVar "x") (PVar "xs")] (EVar "x"),
          DeclFun "h" [PList [PVar "a", PVar "b", PVar "c"]] (EVar "a")
        ]
  let decls =
        [ DeclTypeSig
            "myprint"
            (TArrow (TCon "String") (TCon "String")),
          DeclFun
            "myprint"
            [PVar "str"]
            (EBinOp "++" (EVar "str") (EString "abc"))
        ]
  let decls =
        [ DeclTypeSig "myprint" (TArrow (TCon "String") (TCon "String")),
          DeclFun "myprint" [PVar "str"] (EVar "str")
        ]
-}
