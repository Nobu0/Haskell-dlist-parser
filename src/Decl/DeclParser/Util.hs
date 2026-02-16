{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Util
  ( parensI,
    identI,
    moduleName,
    tokdot,
    operatorI,
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
import Parser.Expr.PatternParser (pattern, patternParser)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

identI :: Parser String
identI = ident <|> typeIdent

parensI :: Parser a -> Parser a
parensI p = symbol "(" *> p <* symbol ")"

moduleName :: Parser String
moduleName = intercalate "." <$> sepBy1 identI tokdot

tokdot :: Parser String
tokdot = token TokDot *> pure "."

operatorI :: Parser Name
operatorI = satisfyToken isOp
  where
    isOp (TokOperator s) = Just s
    isOp _ = Nothing
