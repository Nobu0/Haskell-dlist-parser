-- module Parser.Core.Parser (parseExpr, runToplevelTest, runExprTest, toplevel) where
module Parser.Core.Parser (parseExpr, toplevel) where

import AST.Expr
import Control.Applicative (many, (<|>))
import Lexer.Token (Token (..))
import Lexer.Lexer (runLexer)
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.ExprExtensions (expr, letExpr)
import Parser.Expr.PatternParser
-- import Parser.Type.TypeParser

-- === 実行関数 ===

toplevel :: Parser (String, Expr)
toplevel = do
  name <- ident
  symbol "="
  body <- exprCore
  return (name, body)
{-}
parseExpr :: [Token] -> IO (Maybe Expr)
parseExpr toks = case runParser expr toks of
  Just (result, rest) | null rest -> return (Just result)
  _ -> return Nothing
-}
parseExpr :: [Token] -> Maybe (Expr, [Token])
parseExpr toks = runParser expr toks

{-}
parseExpr :: [Token] -> Maybe Expr
parseExpr toks = case runParser expr toks of
  Just (result, rest) | null rest -> Just result
  _ -> Nothing
-}

parseToplevel :: [Token] -> IO (Maybe (String, Expr))
parseToplevel toks = case runParser toplevel toks of
  Just (result, rest) | null rest -> return (Just result)
  _ -> return Nothing

{-}
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
-}

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
