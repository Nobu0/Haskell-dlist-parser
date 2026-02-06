module Main where

-- import Parser.Parser (parseProgram)

import AST.Expr (Expr)
import Layout.LayoutTransform (layoutTransform)
import Lexer.LayoutLexer (layoutLexer)
import Lexer.Lexer (runLexer)
import Lexer.Token (Token (..))
import Parser.Core.Combinator (Parser (..), runParser)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> processFile file
    _ -> putStrLn "Usage: myparser <filename>"

processFile :: FilePath -> IO ()
processFile fileName = do
  src <- readFile fileName
  putStrLn $ "\n-- file --" ++ src
  -- print src

  -- 自前 Lexer
  let toks1 = runLexer src
  putStrLn "\n-- Tokens from SimpleLexer --"
  print toks1

  -- LayoutLexer
  let toks2 = layoutLexer toks1
  putStrLn "\n-- Tokens from LayoutLexer --"
  print toks2

  -- LayoutTransform (DGC)
  let toks3 = layoutTransform toks2
  putStrLn "\n-- Tokens from LayoutTransform --"
  print toks3

-- Parser（今はまだ外す）
-- let ast = runParser toks3
-- print ast
{-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> processFile file
    _ -> putStrLn "Usage: myparser <filename>"

processFile :: FilePath -> IO ()
processFile file = do
  src <- readFile file

  putStrLn "-- Raw Source --"
  putStrLn src

  let tokens1 = runLexer src
  putStrLn "\n-- Tokens from Lexer --"
  print tokens1

  case tokens1 of
    Left err -> do
      putStrLn "\nLexer error:"
      print err
    Right toks1 -> do
      let tokens2 = layoutLexer toks1
      putStrLn "\n-- Tokens from LayoutLexer --"
      print tokens2

      let tokens3 = layoutTransform tokens2
      putStrLn "\n-- Tokens after LayoutTransform --"
      print tokens3

      putStrLn "\n-- Parsing --"
      case runParser tokens3 of
        Just ast -> do
          putStrLn "\n-- Parsed AST --"
          print ast
        Nothing ->
          putStrLn "Parse error"

  putStrLn "\n-- Parsing --"
  case parseProgram tokens3 of
    Just ast -> do
      putStrLn "\n-- Parsed AST --"
      print ast
    Nothing ->
      putStrLn "Parse error"
-}
