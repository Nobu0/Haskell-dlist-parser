{-}
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
-}
inferCase ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [CaseAlt] ->
  Either InferError (Subst, Type)
inferCase inferExprFn env scrut branches = do
  (sScrut, tScrut) <- inferExprFn env scrut
  results <- mapM (inferBranch inferExprFn env tScrut sScrut) branches
  unifyManyExpr results
