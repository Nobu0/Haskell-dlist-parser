module Main where

import AST.Decl
import AST.Expr
import AST.Pattern
import TypeInference.Infer
import TypeInference.TypeEnv

main :: IO ()
main = do
  -- h [] = 0
  -- h (x:xs) = x + h xs
  let decls = []
  {-}
          [ DeclFun "h" [PList []] (EInt 0),
            DeclFun
              "h"
              [PCons (PVar "x") (PVar "xs")]
              ( EApp
                  (EApp (EVar "+") (EVar "x"))
                  (EApp (EVar "h") (EVar "xs"))
              )
          ]
  -}
  case inferProgram emptyEnv decls of
    Left err -> putStrLn ("Error: " ++ show err)
    Right env -> putStrLn ("Success: " ++ show env)

{-}
module Main where

import AST.Decl
import AST.Expr
import AST.Pattern
import AST.Type
import TypeInference.Infer
import TypeInference.TypeEnv

main :: IO ()
main = do
  let decls =
        [ DeclFun "f" [PConstr "Just" [PVar "x"]] (EVar "x"),
          DeclFun "g" [PCons (PVar "x") (PVar "xs")] (EVar "x"),
          DeclFun "h" [PList [PVar "a", PVar "b", PVar "c"]] (EVar "a")
        ]

  case inferProgram emptyEnv decls of
    Left err -> putStrLn ("Error: " ++ show err)
    Right env -> putStrLn ("Success: " ++ show env)
-}

{-}
import AST.Decl
import AST.Expr
import AST.Pattern
import AST.Type
import TypeInference.Infer
import TypeInference.TypeEnv

main :: IO ()
main = do
  let decls =
        [ DeclTypeSig
            "myprint"
            (TArrow (TCon "String") (TCon "String")),
          DeclFun
            "myprint"
            [PVar "str"]
            (EBinOp "++" (EVar "str") (EString "abc"))
        ]

  case inferProgram emptyEnv decls of
    Left err -> putStrLn ("Error: " ++ show err)
    Right env -> putStrLn ("Success: " ++ show env)

-}
{-}
import AST.Decl
import AST.Expr (Expr (..))
import AST.Pattern
import AST.Type (Type (..))
import TypeInference.Infer
import TypeInference.TypeEnv

main :: IO ()
main = do
  let decls =
        [ DeclTypeSig "myprint" (TArrow (TCon "String") (TCon "String")),
          DeclFun "myprint" [PVar "str"] (EVar "str")
        ]

  case inferProgram emptyEnv decls of
    Left err -> putStrLn ("Error: " ++ show err)
    Right env -> putStrLn ("Success: " ++ show env)
-}

{-}
-- test/Main.hs
import TypeInference.Unify
import AST.Type
import qualified Data.Map as M

main :: IO ()
main = do
  let t1 = TArrow (TVar "a") (TCon "Int")
      t2 = TArrow (TCon "Bool") (TCon "Int")
  case unify t1 t2 of
    Right subst -> putStrLn $ "Unify success: " ++ show subst
    Left err    -> putStrLn $ "Unify failed: " ++ show err
-}

{-}
-- import Parser.Parser (parseProgram)

import AST.Expr (Expr)
import Decl.DeclParserCore (program)
import Layout.LayoutTransform (layoutTransform)
import Lexer.LayoutLexer (layoutLexer)
import Lexer.Lexer (runLexer)
import Lexer.Token (Token (..))
import Parser.Core.Combinator (Parser (..), runParser)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (readFile)
import Utils.MyTrace (setTrace)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> processFile file
    _ -> putStrLn "Usage: myparser <filename>"

processFile :: FilePath -> IO ()
processFile fileName = do
  src <- readFile fileName
  putStrLn $ "\n-- file -- cat  " ++ src
  -- print src

  -- 自前 Lexer
  let toks1 = runLexer src
  -- putStrLn "\n-- Tokens from SimpleLexer --"
  -- print toks1

  -- LayoutLexer
  let toks2 = layoutLexer toks1
  -- putStrLn "\n-- Tokens from LayoutLexer --"
  -- print toks2

  -- LayoutTransform (DGC)
  let toks3 = layoutTransform toks2
  putStrLn "\n-- Tokens from LayoutTransform --"
  print toks3

  -- Parser（今はまだ外す）
  -- let ast = runParser toks3
  result <- runParserWithTrace program toks3
  case result of
    Nothing -> do
      putStrLn "Parse failed"
      exitFailure
    Just decls -> do
      putStrLn "Parsed declarations:"
      mapM_ print decls

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
    {-}
            case result of
              Nothing ->
                putStrLn "Parse failed"
                exitFailure
              Just decls ->
                putStrLn "Parsed declarations:"
                mapM_ print decls
    -}
    _ -> do
      putStrLn "Usage: myapp <source-file>"
      exitFailure
-}

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
