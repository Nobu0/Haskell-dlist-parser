
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
import Language.TypeSystem.Utils.MyTrace
import Language.TypeSystem.Pattern
import Language.TypeSystem.Prelude (initialEnv)
import Language.TypeSystem.Pretty (ppType, printInferResult)
import Language.TypeSystem.Syntax
import Language.TypeSystem.Utils.Utils (simplifyPreds)

fromList = Map.fromList

data TestCase = TestCase
  { testLabel :: String,
    testExpr :: Expr
  }

runInferWithPrimitives :: Expr -> IO (Either InferError (Subst, [Pred], Type))
runInferWithPrimitives expr =
  evalInferM initialEnv (inferExpr expr)

runTestCase :: TestCase -> IO ()
runTestCase (TestCase label expr) = do
  putStrLn $ "\n== Test: " ++ label ++ " =="
  testInfer expr

main :: IO ()
main = do
  setTrace False
  -- setTrace True
  mapM_ runTestCase testCases

testInfer :: Expr -> IO ()
testInfer expr = do
  setTrace False
  -- setTrace True
  putStrLn $ "Input expression:\n" ++ show expr
  result <- runInferWithPrimitives expr -- IO を取り出す
  case result of
    Left err -> putStrLn $ "Type error: " ++ show err
    Right (s, ps, t) -> do
      putStrLn "\nRESULT Substitution:"
      print s
      putStrLn "\nRESULT Predicates:"
      print ps
      putStrLn "\nRESULT Type:"
      print t

{-}
-- | メイン関数：すべてのテストを実行
main :: IO ()
main = mapM_ runTest testCases
  where
    runTest (label, expr) = do
      putStrLn $ "\n== Test: " ++ label ++ " =="
      testInfer expr
-}

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

-- 空レコード
testEmptyRecord = TestCase "EmptyRecord" (ERecord Map.empty)

-- 型注釈付きレコード
testAnnotatedRecord =
  TestCase "AnnotatedRecord" $
    EAnn
      (ERecord (fromList [("x", EInt 1), ("y", EBool True)]))
      (TRecord (fromList [("x", TCon "Int"), ("y", TCon "Bool")]) Nothing)

-- ネストされたフィールドアクセス
testNestedFieldAccess =
  TestCase "NestedFieldAccess" $
    ELet
      (PVar "r")
      ( ERecord
          ( Map.fromList
              [ ( "user",
                  ERecord
                    ( Map.fromList
                        [ ("age", EInt 5),
                          ("name", EString "Mica")
                        ]
                    )
                )
              ]
          )
      )
      (EFieldAccess (EFieldAccess (EVar "r") "user") "age")

-- \(x, y) -> x + y
testLamTuplePattern :: TestCase
testLamTuplePattern =
  TestCase
    "Lam: \\(x, y) -> x + y"
    (ELam [PTuple [PVar "x", PVar "y"]] (EBinOp Add (EVar "x") (EVar "y")))

-- \((x, (y, z)) -> x + z
testLamNestedTuplePattern :: TestCase
testLamNestedTuplePattern =
  TestCase
    "Lam: \\(x, (y, z)) -> x + z"
    ( ELam
        [PTuple [PVar "x", PTuple [PVar "y", PVar "z"]]]
        (EBinOp Add (EVar "x") (EVar "z"))
    )

-- \{x = a, y = b} -> a * b
testLamRecordPattern :: TestCase
testLamRecordPattern =
  TestCase
    "Lam: \\{x = a, y = b} -> a * b"
    ( ELam
        [PRecord [("x", PVar "a"), ("y", PVar "b")]]
        (EBinOp Mul (EVar "a") (EVar "b"))
    )

-- \(x, _) -> x
testLamWildcardPattern :: TestCase
testLamWildcardPattern =
  TestCase
    "Lam: \\(x, _) -> x"
    (ELam [PTuple [PVar "x", PWildcard]] (EVar "x"))

-- \x (y, z) -> z
testLamMultiPattern :: TestCase
testLamMultiPattern =
  TestCase
    "Lam: \\x (y, z) -> z"
    (ELam [PVar "x", PTuple [PVar "y", PVar "z"]] (EVar "z"))

-- \(p@(x, y)) -> x
testLamAsPattern :: TestCase
testLamAsPattern =
  TestCase
    "Lam: \\p@(x, y) -> x"
    (ELam [PAs "p" (PTuple [PVar "x", PVar "y"])] (EVar "x"))

-- case (1, 2) of { p@(x, y) -> x }
testCaseAsPattern :: TestCase
testCaseAsPattern =
  TestCase
    "Case: case (1, 2) of { p@(x, y) -> x }"
    ( ECase
        (ETuple [EInt 1, EInt 2])
        [CaseAlt (PAs "p" (PTuple [PVar "x", PVar "y"])) (EVar "x")]
    )

-- \case { [] -> 0; [x] -> x }
testCaseListPattern1 :: TestCase
testCaseListPattern1 =
  TestCase
    "Case: [] and [x]"
    ( ELambdaCase
        [ CaseAlt (PList []) (EInt 0),
          CaseAlt (PList [PVar "x"]) (EVar "x")
        ]
    )

-- \case { x:xs -> x }
testCaseConsPattern :: TestCase
testCaseConsPattern =
  TestCase
    "Case: x:xs"
    ( ELambdaCase
        [CaseAlt (PCons (PVar "x") (PVar "xs")) (EVar "x")]
    )

-- \case { [] -> 0; x:xs -> x }
testCaseListMatch :: TestCase
testCaseListMatch =
  TestCase
    "Case: [] vs x:xs"
    ( ELambdaCase
        [ CaseAlt (PList []) (EInt 0),
          CaseAlt (PCons (PVar "x") (PVar "xs")) (EVar "x")
        ]
    )

-- \[x, y] -> x + y
testLamListPattern :: TestCase
testLamListPattern =
  TestCase
    "Lam: \
    \[x, y] -> x + y"
    (ELam [PList [PVar "x", PVar "y"]] (EBinOp Add (EVar "x") (EVar "y")))

-- \(x:xs) -> x
testLamConsPattern :: TestCase
testLamConsPattern =
  TestCase
    "Lam: \\(x:xs) -> x"
    (ELam [PCons (PVar "x") (PVar "xs")] (EVar "x"))

-- \case { x :+: y -> x }
testCaseInfixPattern :: TestCase
testCaseInfixPattern =
  TestCase
    "Case: x :+: y"
    ( ELambdaCase
        [CaseAlt (PInfix (PVar "x") ":+:" (PVar "y")) (EVar "x")]
    )

-- \case { Pair x y -> x }
testCaseAppPattern :: TestCase
testCaseAppPattern =
  TestCase
    "Case: Pair x y"
    ( ELambdaCase
        [CaseAlt (PApp (PConstr "Pair" []) [PVar "x", PVar "y"]) (EVar "x")]
    )

-- \case { p@(x :+: xs) -> p }
testCaseAsInfixPattern :: TestCase
testCaseAsInfixPattern =
  TestCase
    "Case: p@(x :+: xs)"
    ( ELambdaCase
        [CaseAlt (PAs "p" (PInfix (PVar "x") ":+:" (PVar "xs"))) (EVar "p")]
    )

-- 期待される型: Num a => a -> a

testCases :: [TestCase]
testCases =
  [ TestCase "EInt 42" (EInt 42),
    TestCase "EBool True" (EBool True),
    TestCase "EChar 'x'" (EChar 'x'),
    TestCase "EString \"hello\"" (EString "hello"),
    TestCase "EUnit" EUnit,
    TestCase "App: (\\x -> x) 42" (EApp (ELam [PVar "x"] (EVar "x")) (EInt 42)),
    TestCase "Lam: \\x -> x" (ELam [PVar "x"] (EVar "x")),
    TestCase "Let: let x = 42 in x" (ELet (PVar "x") (EInt 42) (EVar "x")),
    TestCase "If: if True then 1 else 2" (EIf (EBool True) (EInt 1) (EInt 2)),
    TestCase "Ann: (42 :: Int)" (EAnn (EInt 42) (TCon "Int")),
    TestCase "List: [1, 2, 3]" (EList [EInt 1, EInt 2, EInt 3]),
    TestCase "Case with guards" testCaseGuard,
    TestCase "Tuple: (\\x y -> (x, y)) 1 True" testTuple,
    TestCase "OpSectionL: (+ 1)" testSectionL,
    TestCase "OpSectionR: (1 +)" testSectionR,
    TestCase "LetBlock: let { x = 1; y = x + 2 } in x * y" testLetBlock,
    TestCase "Where: x * y where { x = 2; y = 3 }" testWhere,
    TestCase "LambdaCase: \\case { Just x -> x + 1; Nothing -> 0 }" testLambdaCase,
    TestCase "Record: { x = 1, y = True }" testInfer1,
    TestCase "FieldAccess: ({ x = 42, y = True }).x" testInfer2,
    TestCase "RecordUpdate: ({ x = 1, y = 2 }) { x = 100 }" testInfer3,
    TestCase "LambdaWithFieldAccess: \\r -> r.x + 1" testInfer4,
    TestCase "Nested Records: { user = { name = \"Mica\", age = 5 }, active = True }" testInfer5,
    TestCase "FieldAccess on Lambda: (\\r -> r.x) { x = 1, y = 2 }" testInfer6,
    testEmptyRecord,
    testAnnotatedRecord,
    testNestedFieldAccess,
    testLamTuplePattern,
    testLamNestedTuplePattern,
    testLamRecordPattern,
    testLamWildcardPattern,
    testLamMultiPattern,
    testLamAsPattern,
    testCaseAsPattern,
    testCaseListPattern1,
    testCaseConsPattern,
    testCaseListMatch,
    testLamListPattern,
    testLamConsPattern,
    testCaseInfixPattern,
    testCaseAppPattern,
    testCaseAsInfixPattern
  ]