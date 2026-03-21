{-# LANGUAGE OverloadedStrings #-}

module Main where

-- あなたの型推論器の関数

import AST.Decl
import AST.Expr
import AST.Pattern
import AST.Type
-- (setTrace)
-- (inferProgram)

-- import TypeInference.Infer

import Control.Monad (forM_, when, zipWithM_)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Decl.DeclParserCore (program)
import Lexer.Lexer (runLexer)
import Lexer.Token (Token)
import Parser.Core.Combinator (Parser (..), runParser, try)
import Prettyprinter
import Prettyprinter (Pretty (..), parens, pretty, (<+>))
import Prettyprinter.Render.Terminal (putDoc)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.IO (readFile)
import Text.Printf (printf)
-- import TypeChecker (emptyEnv, inferExpr, parseExpr, prettyType)
import TypeInference.Error (InferError)
import TypeInference.Infer.Core
import TypeInference.Infer.Expr
import TypeInference.Pretty
import TypeInference.TypeEnv
import TypeInference.TypeEnv (TypeEnv, emptyEnv)
import Utils.MyTrace

main :: IO ()
main = do
  -- content <- T.readFile
  let file = "./test/fun/test14.fun"
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  content <- TIO.hGetContents handle

  let lines' = filter (not . T.null) . map strip . T.lines $ content
  let numbered = zip [1 ..] lines'
  results <- mapM runTest numbered
  putStrLn ""
  putStrLn $ " " ++ show numbered

{-}
  let failures = filter (not . snd) results
  putStrLn ""
  putStrLn $ "OK Passed: " ++ show (length results - length failures)
  putStrLn $ "XX Failed: " ++ show (length failures)
  when (not (null failures)) exitFailure
-}

runTest :: (Int, T.Text) -> IO (Int, Bool)
runTest (n, line)
  | "--" `T.isPrefixOf` line = return (n, True) -- コメント行はスキップ
  | otherwise = do
      putStrLn $ "[" ++ show n ++ "] " ++ T.unpack line

      let toks3 = runLexer (T.unpack line)
      let totalTokens = length toks3
      putStrLn $ "\n-- Token list --\n" ++ show toks3
      setTrace False

      putStrLn "\n-- Parsed AST --"
      case runParser program toks3 of
        Just (decls, remaining) -> do
          let consumed = totalTokens - length remaining
              ratio = fromIntegral consumed / fromIntegral totalTokens :: Double
          putStrLn $ "\n-- Parse Coverage --"
          putStrLn $ "Consumed tokens: " ++ show consumed ++ " / " ++ show totalTokens
          putStrLn $ "Coverage: " ++ show (fromInteger (truncate (ratio * 100)) :: Int) ++ "%"

          putStrLn "\n-- AST (numbered) --"
          zipWithM_ (\i d -> putStrLn $ "[" ++ show i ++ "] " ++ show d) [0 ..] decls

          -- 🧠 型推論をここで実行！
          putStrLn "\n-- Type Inference ------------------------"
          setTrace True
          case runInfer (inferProgram primitiveEnv decls) of
            Left err -> do
              putStrLn "Type inference failed:"
              print err
            -- exitFailure
            Right env -> do
              putStrLn "Type inference succeeded!"
              print env
          putStrLn "\n-- Remaining Tokens ----------------------"
          print remaining
        Nothing -> do
          putStrLn "Parsing failed."
          putStrLn $ "Total tokens: " ++ show totalTokens
      return (n, False)

{-}
      case parseExpr (T.unpack line) of
        Left err -> do
          putStrLn $ "  O Parse error: " ++ err
          return (n, expectError line)
        Right expr -> case inferExpr emptyEnv expr of
          Left err -> do
            putStrLn $ "  X Type error: " ++ err
            return (n, expectError line)
          Right ty -> do
            putStrLn $ "  L Type: " ++ prettyType ty
            return (n, not (expectError line))
-}

-- コメントに「型エラー」と書かれていたらエラーを期待
expectError :: T.Text -> Bool
expectError line = "--" `T.isInfixOf` line && "型エラー" `T.isInfixOf` line

strip :: T.Text -> T.Text
strip = T.dropWhile isSpace . T.dropWhileEnd isSpace
