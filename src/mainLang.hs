module Main where

import qualified Data.Map as Map
-- import Debug.Trace (traceShow)
import Language.TypeSystem.BinOp
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.Error
import Language.TypeSystem.Expr
import Language.TypeSystem.Infer
import Language.TypeSystem.InferM
import Language.TypeSystem.Pattern
import Language.TypeSystem.Prelude (initialEnv)
import Language.TypeSystem.Pretty (ppType, printInferResult)
import Language.TypeSystem.Syntax
import Language.TypeSystem.Utils (simplifyPreds)

runInferWithPrimitives :: Expr -> Either InferError (Subst, [Pred], Type)
runInferWithPrimitives expr =
  evalInferM initialEnv (infer expr)

{-}
-- | テスト用：推論を実行して結果を表示
testInfer :: Expr -> IO ()
testInfer expr = do
  -- let result = runInferWithDataEnv preludeDataEnv (infer expr)
  -- let result = runInferWithDataEnv initialEnv (infer expr)
  let result = runInferWithPrimitives expr
  case result of
    Left err -> putStrLn $ "Type error: " ++ show err
    Right (s, ps, t) -> do
      printInferResult (s, ps, t)
-}
testInfer :: Expr -> IO ()
testInfer expr = do
  putStrLn $ "Input expression:\n" ++ show expr
  -- let tracedExpr = traceShow ("[TEST] Input Expr", expr) expr
  let result = runInferWithPrimitives expr -- tracedExpr
  case result of
    Left err -> putStrLn $ "Type error: " ++ show err
    Right (s, ps, t) -> do
      putStrLn "\nRESULT Substitution:"
      print s
      putStrLn "\nRESULT Predicates:"
      print ps
      putStrLn "\nRESULT Type:"
      print t

-- | メイン関数：すべてのテストを実行
main :: IO ()
main = mapM_ runTest testCases
  where
    runTest (label, expr) = do
      putStrLn $ "\n== Test: " ++ label ++ " =="
      testInfer expr

-- putStrLn $ "Type: " ++ show t
-- putStrLn $ "Subst: " ++ show s
-- putStrLn $ "Preds: " ++ show ps

testCaseGuard :: Expr
testCaseGuard =
  ELet (PVar "x") (EInt 42) $
    ECase
      (EVar "x")
      [ CaseAltGuard
          (PVar "n")
          [ (EApp (EApp (EVar ">") (EVar "n")) (EInt 0), EInt 1),
            (EBool True, EInt 0)
          ]
      ]

-- (\x y -> (x, y)) 1 True
testTuple =
  EApp
    ( EApp
        (ELam [PVar "x"] (ELam [PVar "y"] (ETuple [EVar "x", EVar "y"])))
        (EInt 1)
    )
    (EBool True)

-- (+ 1)
testSectionL = EOpSectionL Add (EInt 1)

-- 期待される型: Num a => a -> a

-- (1 +)
testSectionR = EOpSectionR (EInt 1) Add

-- let { x = 1; y = x + 2 } in x * y

testLetBlock =
  ELetBlock
    [ (PVar "x", EInt 1),
      (PVar "y", EBinOp Add (EVar "x") (EInt 2))
    ]
    (EBinOp Mul (EVar "x") (EVar "y"))

-- x * y where { x = 2; y = 3 }
testWhere =
  EWhere
    (EBinOp Mul (EVar "x") (EVar "y"))
    [ (PVar "x", EInt 2),
      (PVar "y", EInt 3)
    ]

-- \case { Just x -> x + 1; Nothing -> 0 }
testLambdaCase =
  ELambdaCase
    [ CaseAlt (PConstr "Just" [PVar "x"]) (EBinOp Add (EVar "x") (EInt 1)),
      CaseAlt (PConstr "Nothing" []) (EInt 0)
    ]

-- { x = 1, y = True }
testInfer1 =
  ERecord
    ( Map.fromList
        [ ("x", EInt 1),
          ("y", EBool True)
        ]
    )

-- ({ x = 42, y = True }).x
testInfer2 =
  EFieldAccess
    ( ERecord
        ( Map.fromList
            [ ("x", EInt 42),
              ("y", EBool True)
            ]
        )
    )
    "x"

-- ({ x = 1, y = 2 }) { x = 100 }
testInfer3 =
  ERecordUpdate
    ( ERecord
        ( Map.fromList
            [ ("x", EInt 1),
              ("y", EInt 2)
            ]
        )
    )
    [("x", EInt 100)]

-- \r -> r.x + 1
testInfer4 =
  ELam
    [PVar "r"]
    ( EBinOp
        Add
        (EFieldAccess (EVar "r") "x")
        (EInt 1)
    )

-- { user = { name = "Mica", age = 5 }, active = True }
testInfer5 =
  ( ERecord
      ( Map.fromList
          [ ( "user",
              ERecord
                ( Map.fromList
                    [ ("name", EString "Mica"),
                      ("age", EInt 5)
                    ]
                )
            ),
            ("active", EBool True)
          ]
      )
  )

-- (\r -> r.x) { x = 1, y = 2 }
testInfer6 =
  ( EApp
      (ELam [PVar "r"] (EFieldAccess (EVar "r") "x"))
      (ERecord (Map.fromList [("x", EInt 1), ("y", EInt 2)]))
  )

-- 期待される型: Num a => a -> a

testCases :: [(String, Expr)]
testCases =
  [ ("EInt 42", EInt 42),
    ("EBool True", EBool True),
    ("EChar 'x'", EChar 'x'),
    ("EString \"hello\"", EString "hello"),
    ("EUnit", EUnit),
    -- 複雑な式のテスト
    ("App: (\\x -> x) 42", EApp (ELam [PVar "x"] (EVar "x")) (EInt 42)),
    ("Lam: \\x -> x", ELam [PVar "x"] (EVar "x")),
    ("Let: let x = 42 in x", ELet (PVar "x") (EInt 42) (EVar "x")),
    ("If: if True then 1 else 2", EIf (EBool True) (EInt 1) (EInt 2)),
    ("Ann: (42 :: Int)", EAnn (EInt 42) (TCon "Int")),
    ("List: [1, 2, 3]", EList [EInt 1, EInt 2, EInt 3]),
    ( "Case: case True of { True -> 1; False -> 0 }",
      ECase
        (EBool True)
        [ CaseAlt (PVar "True") (EInt 1),
          CaseAlt (PVar "False") (EInt 0)
        ]
    ),
    ("Case with guards", testCaseGuard),
    ("Tuple: (\\x y -> (x, y)) 1 True", testTuple),
    ("OpSectionL: (+ 1)", testSectionL),
    ("OpSectionR: (1 +)", testSectionR),
    ("LetBlock: let { x = 1; y = x + 2 } in x * y", testLetBlock),
    ("Where: x * y where { x = 2; y = 3 }", testWhere),
    ("LambdaCase: \\case { Just x -> x + 1; Nothing -> 0 }", testLambdaCase),
    ("Record: { x = 1, y = True }", testInfer1),
    ("FieldAccess: ({ x = 42, y = True }).x", testInfer2),
    ("RecordUpdate: ({ x = 1, y = 2 }) { x = 100 }", testInfer3),
    ("LambdaWithFieldAccess: \\r -> r.x + 1", testInfer4),
    ("Nested Records: { user = { name = \"Mica\", age = 5 }, active = True }", testInfer5),
    ("FieldAccess on Lambda: (\\r -> r.x) { x = 1, y = 2 }", testInfer6)
  ]
