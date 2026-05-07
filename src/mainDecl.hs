module Main where

import qualified Data.Map as Map
-- import Debug.Trace (traceShow)
import Language.TypeSystem.BinOp
import Language.TypeSystem.DataEnv
import Language.TypeSystem.Decl
import Language.TypeSystem.Env
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

fromList :: (Ord k) => [(k, v)] -> Map.Map k v
fromList = Map.fromList

data TestCase = TestCase
  { testLabel :: String,
    testDecl :: Decl
  }

runInferWithPrimitives :: Decl -> IO (Either InferError (Subst, [Pred], TypeEnv))
runInferWithPrimitives decl =
  evalInferM initialEnv (inferDecl decl)

{-}
runTestCase :: TestCase -> IO ()
runTestCase (TestCase label decl) = do
  putStrLn $ "\n== Test: " ++ label ++ " =="
  testInfer decl
-}
runTestCase :: TestCase -> IO ()
runTestCase (TestCase label decl) = do
  putStrLn $ "\n== Test: " ++ label ++ " =="
  result <- runInferWithPrimitives decl -- IO の中身を取り出す
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (s, ps, env) -> do
      putStrLn "\nRESULT Substitution:"
      print s
      putStrLn "\nRESULT Predicates:"
      print ps
      putStrLn "\nRESULT TypeEnv:"
      print env

main :: IO ()
main = do
  setTrace False
  -- setTrace True
  mapM_ runTestCase testCases

testInfer :: Decl -> IO ()
testInfer decl = do
  setTrace False
  -- setTrace True
  putStrLn $ "Input declaration:\n" ++ show decl
  result <- runInferWithPrimitives decl -- IO を取り出す
  case result of
    Left err -> putStrLn $ "Type error: " ++ show err
    Right (s, ps, env) -> do
      putStrLn "\nRESULT Substitution:"
      print s
      putStrLn "\nRESULT Predicates:"
      print ps
      putStrLn "\nRESULT TypeEnv:"
      print env

{-}
-- | メイン関数：すべてのテストを実行
main :: IO ()
main = mapM_ runTest testCases
  where
    runTest (label, expr) = do
      putStrLn $ "\n== Test: " ++ label ++ " =="
      testInfer expr
-}
-- f x y = x + y
testFunGroup :: TestCase
testFunGroup =
  TestCase "f x y = x+y    (funGroup)" $
    DeclFunGroup
      "f"
      [FunClause [PVar "x", PVar "y"] Nothing (Just (EBinOp Add (EVar "x") (EVar "y"))) Nothing]

testFunWithSig :: TestCase
testFunWithSig =
  TestCase "f :: Int -> Int; f x = x + 1  (funWithSig)" $
    DeclFunGroup
      "f"
      [ FunClause
          [PVar "x"]
          Nothing
          (Just (EBinOp Add (EVar "x") (EInt 1)))
          Nothing
      ]

{-}
testFunWithSig2 :: TestCase
testFunWithSig2 =
  TestCase "f :: Int -> Int; f x = x + 1" $
    DeclGroup
      [ DeclTypeSig "f" (Forall [] [IsIn "Num" (TCon "Int")] (TArrow (TCon "Int") (TCon "Int"))),
        DeclFunGroup "f" [FunClause [PVar "x"] Nothing (Just (EBinOp Add (EVar "x") (EInt 1))) Nothing]
      ]
-}

-- f x = x
testId :: TestCase
testId =
  TestCase "f x = x  (id)" $
    DeclFunGroup
      "f"
      [ FunClause
          [PVar "x"]
          Nothing
          (Just (EVar "x"))
          Nothing
      ]

-- f x y = x + y
testAdd :: TestCase
testAdd =
  TestCase "f x y = x + y   (add)" $
    DeclFunGroup
      "f"
      [ FunClause
          [PVar "x", PVar "y"]
          Nothing
          (Just (EBinOp Add (EVar "x") (EVar "y")))
          Nothing
      ]

-- f :: Int -> Int; f x = x + 1
testWithTypeSig :: TestCase
testWithTypeSig =
  TestCase "f :: Int -> Int; f x = x + 1   (withTypeSig)" $
    -- [ DeclTypeSig "f" (Forall [] [IsIn "Num" (TCon "Int")] (TArrow (TCon "Int") (TCon "Int"))),
    DeclFunGroup
      "f"
      [ FunClause
          [PVar "x"]
          Nothing
          (Just (EBinOp Add (EVar "x") (EInt 1)))
          Nothing
      ]

-- ]

-- f x = x + True  -- 型エラーになるべき
testTypeError :: TestCase
testTypeError =
  TestCase "f x = x + True (should fail)" $
    DeclFunGroup
      "f"
      [ FunClause
          [PVar "x"]
          Nothing
          (Just (EBinOp Add (EVar "x") (EBool True)))
          Nothing
      ]

-- 期待される型: Num a => a -> a

testCases :: [TestCase]
testCases =
  [ testFunGroup,
    testFunWithSig,
    testId,
    testAdd,
    testWithTypeSig,
    testTypeError
  ]
