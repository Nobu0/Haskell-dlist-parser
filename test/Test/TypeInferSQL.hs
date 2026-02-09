module Main where

-- module Test.TypeInferSQL where

import Lexer.Lexer (runLexer)
-- import Parser.Core.Combinator (Parser (..), runParser)
import Parser.Core.Parser (parseExpr, toplevel)
-- import Parser.Expr.ExprParser (exprTop)

import TypeInference.Infer
import TypeInference.Infer (inferExpr)
import TypeInference.Pretty (prettyType)
import TypeInference.Type (Type (..))
import TypeInference.TypeEnv (emptyEnv)

runInfer :: String -> String -> IO ()
runInfer cm src = do
  putStrLn $ "\n-- test " ++ cm ++ " Input: " ++ src
  let toks = runLexer src
  putStrLn $ "Tokens: " ++ show toks

  let parsed = parseExpr toks
  case parsed of
    Nothing -> putStrLn "Parse failed"
    Just (ast, _) -> do
      putStrLn $ "AST: " ++ show ast
      case inferExpr emptyEnv ast of
        Left err -> putStrLn $ "TypeError: " ++ show err
        Right (_, ty) -> putStrLn $ "Type: " ++ prettyType ty
        _ -> putStrLn $ "error"

-- テストケース群
testSQLInfer :: IO ()
testSQLInfer = do
  -- sqlti1
  runInfer "sqlt1" "sql \"SELECT * FROM t\""

  -- sqlti2
  runInfer "sqlt2" "let x = 10 in sql \"SELECT * FROM t WHERE id = {x}\""

  -- sqlti3
  runInfer "sqlt3" "let a = 1 in let x = sql \"SELECT {a}\" in x"

  -- sqlti4
  runInfer "sqlt4" "let a = 1 in let f = (sql \"SELECT {a}\") in f"

  -- sqlti5
  runInfer "sqlt5" "let a = 1 in a where y = sql \"SELECT {a}\""

  -- sqlti6
  runInfer "sqlt6" "do { let a = 1 in let x = (sql \"SELECT {a}\") in x }"

  -- sqlti7（型エラーになるべき）
  runInfer "sqltERROR" "(sql \"SELECT {a}\") + (sql \"SELECT {b}\")"

  -- sqlti8
  -- runInfer "sqlt8" "let x = 1 where f b = (sql \"SELECT {b}\") in f x"
  -- sqlti8
  runInfer "sqlt8" "do {let (a,b) = (1,2) in let f = (sql \"SELECT {a} {b}\") in f}"

main :: IO ()
main = testSQLInfer
