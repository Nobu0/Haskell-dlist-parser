module Main where

import Language.TypeSystem.Syntax
import Language.TypeSystem.Expr
import Language.TypeSystem.Infer
import Language.TypeSystem.DataEnv
import Language.TypeSystem.InferM
import Language.TypeSystem.Error

-- | テスト用：推論を実行して結果を表示
testInfer :: Expr -> IO ()
testInfer expr = do
  let result = runInferWithDataEnv preludeDataEnv (infer expr)
  case result of
    Left err -> putStrLn $ "Type error: " ++ show err
    Right (s, ps, t) -> do
      putStrLn $ "Type: " ++ show t
      putStrLn $ "Subst: " ++ show s
      putStrLn $ "Preds: " ++ show ps

-- | テスト対象の式たち
testCases :: [(String, Expr)]
testCases =
  [ ("EInt 42", EInt 42)
  , ("EBool True", EBool True)
  , ("EChar 'x'", EChar 'x')
  , ("EString \"hello\"", EString "hello")
  , ("EUnit", EUnit)
  ]

-- | メイン関数：すべてのテストを実行
main :: IO ()
main = mapM_ runTest testCases
  where
    runTest (label, expr) = do
      putStrLn $ "\n== Test: " ++ label ++ " =="
      testInfer expr
