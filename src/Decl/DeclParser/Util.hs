{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Util
  ( parensI,
    identI,
    moduleName,
    tokdot,
    operatorI,
    sepBy1Skip,
    skipBlk,
    typeExpr,
    operatorIAsName,
  )
where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (Constraint), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (intercalate)
-- ★ ここが正しい

-- (keyword) -- , whereClause)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

parensI :: Parser a -> Parser a
parensI p = symbol "(" *> p <* symbol ")"

moduleName :: Parser String
moduleName = intercalate "." <$> sepBy1 identI tokdot

tokdot :: Parser String
tokdot = token TokDot *> pure "."

typeExpr :: Parser Type
typeExpr = do
  ts <- some typeAtom
  return $ foldl1 TApp ts

sepBy1Skip :: Parser a -> Parser sep -> Parser [a]
sepBy1Skip p sep = do
  -- t <- lookAhead anyToken
  -- myTrace ("<< sepBy1Skip: next token " ++ show t)
  skipControlTokens
  x <- p
  xs <- many $ do
    skipControlTokens
    _ <- sep
    skipControlTokens
    p
  skipControlTokens
  optional sep
  skipControlTokens
  return (x : xs)

isControlToken :: Token -> Bool
isControlToken t = case t of
  TokVLBrace -> True
  TokVRBrace -> True
  TokNewline -> True
  TokSymbol ";" -> True
  _ -> False

skipControlTokens :: Parser ()
skipControlTokens = skipMany $ satisfyToken (\t -> if isControlToken t then Just () else Nothing)

skipBlk :: Parser ()
skipBlk = do
  optional (token TokVLBrace)
  optional (token TokVRBrace)
  optional (token TokNewline)
  optional (token $ TokSymbol ";")
  return ()
