module Main where

-- import Parser.Core.Parser (runParser)

-- (runLexer)
-- import Layout.LayoutTransform --
-- import Lexer.LayoutLexer

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.IO as TIO
import Decl.DeclParserCore (program)
import Lexer.Lexer (runLexer)
import Lexer.Token (Token)
import Parser.Core.Combinator (Parser (..), runParser, try)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import System.IO (readFile)
import Utils.MyTrace (setTrace)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> processFile file
    _ -> putStrLn "Usage: myparser <filename>"

processFile :: FilePath -> IO ()
processFile file = do
  handle <- openFile file ReadMode
  hSetEncoding handle utf8 -- ★ ここでエンコーディングを明示！
  src <- TIO.hGetContents handle
  -- src <- T.readFile file

  putStrLn "-- Raw Source --"
  putStrLn (T.unpack src)

  let toks3 = runLexer (T.unpack src)
  putStrLn "\n-- Tokens from Lexer --"
  print toks3
  {-}
    let toks2 = layoutLexer toks1
    putStrLn "\n-- Tokens from LayoutLexer --"
    print toks2

    let toks3 = layoutTransform toks2
    putStrLn "\n-- Tokens after LayoutTransform --"
    print toks3
  -}
  putStrLn "\n-- Parsing --"
  let ast = runParser program toks3
  putStrLn "\n-- Parsed AST --"
  print ast
