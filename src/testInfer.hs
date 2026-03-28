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
-- import TypeChecker (emptyEnv, inferExpr, parseExpr, prettyType)

import Parser.Expr.ExprParser (exprTop)
import Prettyprinter
import Prettyprinter (Pretty (..), parens, pretty, (<+>))
import Prettyprinter.Render.Terminal (putDoc)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.IO (readFile)
import Text.Printf (printf)
import TypeInference.Error (InferError)
import TypeInference.Infer.Core
import TypeInference.Infer.Expr
import TypeInference.Pretty
import TypeInference.TypeEnv
import TypeInference.TypeEnv (TypeEnv, emptyEnv)
import Utils.MyTrace

main :: IO ()
main = do
  let file = "./test/fun/test99.fun"
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  content <- TIO.hGetContents handle

  let blocks = splitBlocks (T.lines content) -- インデント保持！
  let numbered = zip [1 ..] blocks
  results <- mapM runDeclBlock numbered
  putStrLn ""
  putStrLn $ "Ran " ++ show (length results) ++ " tests."
  exitFailure
  let file = "./test/fun/test14.fun"
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  content <- TIO.hGetContents handle

  let blocks = splitBlocks (T.lines content) -- インデント保持！
  let numbered = zip [1 ..] blocks
  results <- mapM runExprTest numbered
  putStrLn ""
  putStrLn $ "Ran " ++ show (length results) ++ " tests."

splitBlocks :: [T.Text] -> [[T.Text]]
splitBlocks = filter (not . null) . foldr step [[]]
  where
    step line (b : bs)
      | T.all isSpace line = [] : b : bs
      | otherwise = (line : b) : bs
    step _ [] = error "unreachable"

runExprTest :: (Int, [T.Text]) -> IO (Int, Bool)
runExprTest (n, lines) = do
  let source = T.unpack (T.unlines lines)
  putStrLn $ "[" ++ show n ++ "]\n" ++ source

  let toks = runLexer source
  putStrLn $ "\n-- Token list --\n" ++ show toks
  case toks of
    [newline] -> return (n, True)
    _ -> do
      setTrace False
      let toks1 = tail toks
      putStrLn "\n-- Parsed Expr --"
      case runParser exprTop toks1 of
        Just (ast, []) -> do
          putStrLn $ " AST " ++ show ast
          putStrLn "\n-- Type Inference ------------------------"
          setTrace True
          case runInfer (inferExpr withTestEnv ast) of
            Left err -> do
              putStrLn "Type inference failed:"
              print err
              exitFailure
              return (n, False)
            Right (subst, ty) -> do
              putStrLn "Type inference succeeded!"
              putStrLn $ "Type: " ++ show ty
              return (n, True)
        _ -> do
          putStrLn "\n-- Expr Parse Error"
          exitFailure
          return (n, False)

runDeclBlock :: (Int, [T.Text]) -> IO (Int, Bool)
runDeclBlock (n, lines) = do
  let source = T.unpack (T.unlines lines)
  putStrLn $ "[" ++ show n ++ "]\n" ++ source

  let toks3 = runLexer source
  let totalTokens = length toks3
  putStrLn $ "\n-- Token list --\n" ++ show toks3
  setTrace False
  --setTrace True

  putStrLn "\n-- Parsed AST --"
  case runParser program toks3 of
    Just (decls, remaining) -> do
      let consumed = totalTokens - length remaining
          ratio = fromIntegral consumed / fromIntegral totalTokens :: Double
      putStrLn $ "\n-- Parse Coverage --"
      putStrLn $ "Consumed tokens: " ++ show consumed ++ " / " ++ show totalTokens
      putStrLn $ "Coverage: " ++ show (truncate (ratio * 100) :: Int) ++ "%"

      putStrLn "\n-- AST (numbered) --"
      zipWithM_ (\i d -> putStrLn $ "[" ++ show i ++ "] " ++ show d) [0 ..] decls

      putStrLn "\n-- Type Inference ------------------------"
      setTrace True
      case runInfer (inferProgram primitiveEnv decls) of
        Left err -> do
          putStrLn "Type inference failed:"
          print err
          return (n, False)
        Right env -> do
          putStrLn "Type inference succeeded!"
          print env
          return (n, True)
    -- putStrLn "\n-- Remaining Tokens ----------------------"
    -- print remaining
    Nothing -> do
      putStrLn "Parsing failed."
      putStrLn $ "Total tokens: " ++ show totalTokens
      return (n, False)

-- コメントに「型エラー」と書かれていたらエラーを期待
expectError :: T.Text -> Bool
expectError line = "--" `T.isInfixOf` line && "型エラー" `T.isInfixOf` line

strip :: T.Text -> T.Text
strip = T.dropWhile isSpace . T.dropWhileEnd isSpace
