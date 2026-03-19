module Main where

import AST.Decl
import AST.Expr
import AST.Pattern
import AST.Type
-- (setTrace)
-- (inferProgram)

-- import TypeInference.Infer

import Control.Monad (forM_, zipWithM_)
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
import TypeInference.Error (InferError)
import TypeInference.Infer.Core
import TypeInference.Infer.Expr
import TypeInference.Pretty
import TypeInference.TypeEnv
import TypeInference.TypeEnv (TypeEnv, emptyEnv)
import Utils.MyTrace

printTypeBinding :: (String, Scheme) -> IO ()
printTypeBinding (name, scheme) = do
  putStrLn $ name ++ " :: " ++ prettyScheme scheme

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      processFile file
    _ -> putStrLn "Usage: myparser <filename>"

processFile :: FilePath -> IO ()
processFile file = do
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  src <- TIO.hGetContents handle
  putStrLn "\n-- src --\n"
  TIO.putStrLn src

  let toks3 = runLexer (T.unpack src)
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
          exitFailure
        Right env -> do
          putStrLn "Type inference succeeded!"
          print env
      putStrLn "\n-- Remaining Tokens ----------------------"
      print remaining
    Nothing -> do
      putStrLn "Parsing failed."
      putStrLn $ "Total tokens: " ++ show totalTokens
