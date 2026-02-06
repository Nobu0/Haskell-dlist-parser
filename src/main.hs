module Main where

-- import Parser.Core.Parser (runParser)
import Decl.DeclParserCore (program)
import Lexer.Lexer (runLexer)
import Lexer.Token (Token)
import Parser.Core.Combinator (Parser (..), runParser, try)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile)
import Utils.MyTrace (setTrace)

-- あなたの既存の関数
runParserWithTrace :: Parser a -> [Token] -> IO (Maybe a)
runParserWithTrace p tokens = do
  -- setTrace False
  setTrace True
  case runParser p tokens of
    Just (result, []) -> return (Just result)
    _ -> do
      putStrLn "XX Parser failed! Re-running with trace:"
      setTrace True
      case runParser p tokens of
        Just (result, []) -> return (Just result)
        _ -> return Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      src <- readFile file
      putStrLn $ "\n-- Input: " ++ src
      let toks = runLexer src
      putStrLn $ "Tokens: " ++ show toks
      result <- runParserWithTrace program toks
      case result of
        Nothing -> do
          putStrLn "Parse failed"
          exitFailure
        Just decls -> do
          putStrLn "Parsed declarations:"
          mapM_ print decls
    _ -> do
      putStrLn "Usage: myapp <source-file>"
      exitFailure
