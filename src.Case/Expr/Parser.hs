module Expr.Parser (parseExpr, runToplevelTest, runExprTest, expr) where

import Expr.AST
import Expr.Combinator
import Expr.ExprParser
import Expr.PatternParser
import Expr.TokenParser
import Expr.TypeParser
import Lexer (Token (..), runLexer)

-- === 実行関数 ===
{-}
parseExpr :: [Token] -> IO (Maybe Expr)
parseExpr toks = do
  -- putStrLn $ "Tokens: " ++ show toks -- ← 追加！
  case runParser toplevel toks of
    --  case runParser expr toks of
    Just (e, rest) -> do
      putStrLn $ "Parsed expression: " ++ show e
      putStrLn $ "Remaining tokens: " ++ show rest
      return (Just e)
    Nothing -> do
      putStrLn "Parsing failed!"
      return Nothing

parseExpr :: [Token] -> IO (Maybe Expr)
parseExpr toks = do
  case runParser toplevel toks of
    Just ((name, expr), rest) -> do
      putStrLn $ "Parsed definition: " ++ name ++ " = " ++ show expr
      putStrLn $ "Remaining tokens: " ++ show rest
      return (Just expr)
    Nothing -> do
      putStrLn "Parsing failed!"
      return Nothing
-}
parseExpr :: [Token] -> IO (Maybe Expr)
parseExpr toks = case runParser expr toks of
  Just (result, rest) | null rest -> return (Just result)
  _ -> return Nothing

parseToplevel :: [Token] -> IO (Maybe (String, Expr))
parseToplevel toks = case runParser toplevel toks of
  Just (result, rest) | null rest -> return (Just result)
  _ -> return Nothing

-- 式のテスト
runExprTest :: ([Char], [Char]) -> IO ()
runExprTest (input, expected) = do
  putStrLn $ "Input: " ++ input
  case runLexer input of
    Left err -> putStrLn $ "  Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ " Tokens: " ++ show tokens
      result <- parseExpr tokens
      case result of
        Nothing -> putStrLn "  Parser error!"
        Just ast ->
          compareAST (show ast) expected

{-}
    Just (e, rest) -> do
      putStrLn $ "Parsed definition: " ++ name ++ " = " ++ show expr
      putStrLn $ "Remaining tokens: " ++ show rest
      return (Just expr)
    Nothing -> do
      putStrLn "Parsing failed!\n"
      return Nothing
-}

-- トップレベル定義のテスト
runToplevelTest :: ([Char], [Char]) -> IO ()
runToplevelTest (input, expected) = do
  putStrLn $ "Input: " ++ input
  case runLexer input of
    Left err -> putStrLn $ "  Lexer error: " ++ show err
    Right tokens -> do
      putStrLn $ " Tokens: " ++ show tokens
      result <- parseToplevel tokens
      case result of
        Nothing -> putStrLn "  Parser error!\n"
        Just (_, ast) ->
          compareAST (show ast) expected

-- 共通の比較処理
compareAST actualRaw expectedRaw = do
  let normalize = filter (not . (`elem` [' ', '\n', '\t']))
      actual = normalize actualRaw
      expected = normalize expectedRaw
  if actual == expected
    then putStrLn "  O Passed\n"
    else do
      putStrLn "  X Failed!"
      putStrLn $ "     Expected: " ++ expectedRaw
      putStrLn $ "     Got:      " ++ actualRaw ++ "\n"
