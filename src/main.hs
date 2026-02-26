module Main where

import AST.BinOp
import AST.BinOp (BinOp (..)) -- 必要に応じてインポート
import AST.Expr
import AST.Type (Type (..))
import Control.Exception (evaluate)
-- (pretty, prettyType)

import Control.Monad (zipWithM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.IO as TIO
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
import TypeInference.Pretty
import Utils.MyTrace
import Control.Monad (forM_)
import Data.List (intercalate)
import Control.Monad (forM_)
import Data.List (intercalate)
import Text.Printf (printf)


printExpr :: Expr -> String
printExpr (EVar name) = name
printExpr (EAnn e ty) = printExpr e ++ " :: " ++ prettyType ty

{-}
instance Pretty Expr where
  pretty (EVar name) = pretty name
  pretty (EInt n) = pretty n
  pretty (EBinOp op l r) =
    parens (pretty l <+> pretty op <+> pretty r)

instance Pretty BinOp where
  pretty BinOpAdd = "+"
  pretty BinOpSub = "-"
  pretty BinOpMul = "*"
  pretty BinOpDiv = "/"
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      processFile file
      processFile2 file
    _ -> putStrLn "Usage: myparser <filename>"

processFile :: FilePath -> IO ()
processFile file = do
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  src <- TIO.hGetContents handle

  let toks3 = runLexer (T.unpack src)
  let totalTokens = length toks3

  -- 最初のパース（トレースOFF）
  setTrace False
  -- setTrace True

  putStrLn "\n-- Parsed AST --"
  case runParser program toks3 of
    Just (ast, remaining) -> do
      let consumed = totalTokens - length remaining
          ratio = fromIntegral consumed / fromIntegral totalTokens :: Double
      putStrLn $ "\n-- Parse Coverage --"
      putStrLn $ "Consumed tokens: " ++ show consumed ++ " / " ++ show totalTokens
      putStrLn $ "Coverage: " ++ show (fromInteger (truncate (ratio * 100)) :: Int) ++ "%"

      putStrLn "\n-- AST----------------------------------"
      -- putDoc (pretty ast <> line)
      putStrLn "\n-- AST (numbered) --"
      zipWithM_ (\i d -> putStrLn $ "[" ++ show i ++ "] " ++ show d) [0 ..] ast
      print ast
      putStrLn "\n-- Remaining-----------------------------"
      print remaining
    Nothing -> do
      putStrLn "Parsing failed."
      putStrLn $ "Total tokens: " ++ show totalTokens

processFile2 :: FilePath -> IO ()
processFile2 file = do
  handle <- openFile file ReadMode
  hSetEncoding handle utf8
  src <- TIO.hGetContents handle

  let toks3 = runLexer (T.unpack src)

  setTrace True
  putStrLn "\n-- Parsed AST log --"
  let ast = runParser program toks3
  print ast
  -- print ast
  putStrLn "\n-- src code --"
  putStrLn (T.unpack src)

  putStrLn "\n-- token --"
  printTokensAligned toks3

-- 表示処理
printTokensInGroups :: Show a => [a] -> IO ()
printTokensInGroups toks =
  forM_ (zip [0,5..] (chunksOf 5 toks)) $ \(i, group) -> do
    let line = "[" ++ show i ++ "] " ++ intercalate ", " (map show group)
    putStrLn line


-- 5個ずつ分割
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (first, rest) = splitAt n xs
   in first : chunksOf n rest

-- 幅をそろえて表示
printTokensAligned :: Show a => [a] -> IO ()
printTokensAligned toks = do
  let formatToken tok = printf "%-20s" (take 20 (show tok))  -- 最大20文字、左詰め
  forM_ (zip [0,5..] (chunksOf 5 toks)) $ \(i, group) -> do
    let line = "[" ++ show i ++ "] " ++ concatMap formatToken group
    putStrLn line
