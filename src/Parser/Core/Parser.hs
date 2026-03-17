-- module Parser.Core.Parser (parseExpr, runToplevelTest, runExprTest, toplevel) where
module Parser.Core.Parser (parseExpr, toplevel) where

import AST.Expr
import Control.Applicative (many, (<|>))
import Lexer.Lexer (runLexer)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprCore (exprCore)
import Parser.Expr.ExprExtensions (expr, letExpr)
import Parser.Expr.PatternParser

-- === 実行関数 ===

toplevel :: Parser (String, Expr)
toplevel = do
  name <- ident
  symbol "="
  body <- exprCore
  return (name, body)

parseExpr :: [Token] -> Maybe (Expr, [Token])
parseExpr toks = runParser expr toks

parseToplevel :: [Token] -> IO (Maybe (String, Expr))
parseToplevel toks = case runParser toplevel toks of
  Just (result, rest) | null rest -> return (Just result)
  _ -> return Nothing

-- 共通の比較処理
compareAST :: [Char] -> [Char] -> IO ()
compareAST actualRaw expectedRaw = do
  let normalize = filter (not . (`elem` [' ', '\n', '\t']))
  let actual = normalize actualRaw
  let expected = normalize expectedRaw
  if actual == expected
    then putStrLn "  O Passed\n"
    else do
      putStrLn "  X Failed!"
      putStrLn $ "     Expected: " ++ expectedRaw
      putStrLn $ "     Got:      " ++ actualRaw ++ "\n"
