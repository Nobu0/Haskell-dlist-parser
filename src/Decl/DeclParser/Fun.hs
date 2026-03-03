{-# LANGUAGE LambdaCase #-}

module Decl.DeclParser.Fun (funDecl) where

import AST.Decl
import AST.Expr
-- import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Constraint (..), Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
-- ★ ここが正しい

-- (keyword) -- , whereClause)

import Control.Monad (guard)
import Data.List (intercalate)
import Decl.DeclParser.Util
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprExtensions -- (expr, exprCore, skipNewlines)
import Parser.Expr.PatternParser (pPattern, pattern)
import Parser.Type.TypeParser (constraintList, parseType, typeAtom, typeIdent, typeP)
import Utils.MyTrace

mkSimpleClause :: [Pattern] -> Expr -> Maybe [Decl] -> FunClause
mkSimpleClause pats body whereDecls =
  FunClause pats Nothing (Just body) whereDecls

mkGuardedClause :: [Pattern] -> [(Expr, Expr)] -> Maybe [Decl] -> FunClause
mkGuardedClause pats guards whereDecls =
  FunClause pats (Just guards) Nothing whereDecls

-- 関数宣言
funDecl :: Parser Decl
funDecl = do
  t <- lookAhead anyToken
  myTrace ("<< funDecl: next token=" ++ show t)
  (name, clause1) <- funClause -- funDecl
  rest <- many (try (funClauseWithName name))
  -- optional (token TokVRBrace)
  myTrace (">>*funDecl name " ++ show name ++ " clause1 " ++ show clause1 ++ " rest " ++ show rest)
  return (DeclFunGroup name (clause1 : rest))

funClause :: Parser (Name, FunClause)
funClause = do
  t0 <- lookAhead anyToken
  myTrace ("<< funClause: next token=" ++ show t0)
  name <- ident
  args <- many pPattern -- patternParser
  skipNewlines
  t <- lookAhead anyToken
  myTrace ("<< funClause: args=" ++ show args ++ " t = " ++ show t)
  case t of
    TokSymbol "=" -> parseSimpleClause name args
    TokSymbol "|" -> parseGuardedClause name args
    _ -> parseGuardedClause name args

parseSimpleClause :: Name -> [Pattern] -> Parser (Name, FunClause)
parseSimpleClause name args = do
  symbol "="
  t <- lookAhead anyToken
  myTrace ("<< parseSimpleClause: next token=" ++ show t)
  bracesV $ do
    -- skipSeparators
    e <- expr <|> exprCore
    w <- optional whereBlock
    return (name, mkSimpleClause args e w)

parseGuardedClause :: Name -> [Pattern] -> Parser (Name, FunClause)
parseGuardedClause name args = do
  t <- lookAhead anyToken
  myTrace ("<< parseGuardedClause: next token=" ++ show t)
  bracesV $ do
    skipSeparators
    guards <- guardedRhs
    w <- optional (whereBlock)
    return (name, mkGuardedClause args guards w)

funDeclGroup :: Parser Decl
funDeclGroup = do
  t <- lookAhead anyToken
  myTrace ("<< funDeclGroup: next token=" ++ show t)
  (name1, clause1) <- funClause
  rest <- many (try (funClauseWithName name1))
  return (DeclFunGroup name1 (clause1 : rest))

-- 同じ名前の関数をグループ化する
funClauseWithName :: Name -> Parser FunClause
funClauseWithName name = try $ do
  skipSeparators
  t <- lookAhead anyToken
  myTrace ("<< funClauseWithName: next token=" ++ show t)
  name' <- ident
  guard (name == name')
  args <- many pattern -- patternParser
  -- skipSeparators
  t <- lookAhead anyToken
  case t of
    TokSymbol "=" -> do
      symbol "="
      bracesV $ do
        skipSeparators
        e <- expr
        -- w <- optional (bracesV (whereBlock))
        w <- optional whereBlock
        return (mkSimpleClause args e w)
    TokSymbol "|" -> do
      guards <- guardedRhs
      w <- optional whereBlock
      return (mkGuardedClause args guards w)
    _ -> bracesV $ do
      skipSeparators
      e <- expr
      -- w <- optional (bracesV (whereBlock))
      w <- optional whereBlock
      return (mkSimpleClause args e w)

whereBlock :: Parser [Decl]
whereBlock = do
  t <- lookAhead anyToken
  myTrace ("<< whereBlock: next token " ++ show t)
  bracesV $ do
    keyword "where"
    skipSeparators
    bracesV $ do
      decls <- many1 $ do
        funDecl
      return decls

guardedRhsM :: Parser [(Expr, Expr)]
guardedRhsM = many1 parseGuardLine

parseGuardLine :: Parser (Expr, Expr)
parseGuardLine = do
  t <- lookAhead anyToken
  myTrace ("<< parseGuardLine: next token=" ++ show t)
  symbol "|"
  cond <- expr
  symbol "="
  body <- expr
  return (cond, body)

guardedRhs :: Parser [(Expr, Expr)]
guardedRhs = do
  many1 $ do
    skipSeparators
    t <- lookAhead anyToken
    myTrace ("<< guardedRhs: next token = " ++ show t)
    parseGuardLine

funHead :: Parser (Name, [Pattern])
funHead = do
  p <- pattern
  myTrace ("<< funHead pattern: " ++ show p)
  case p of
    PVar name -> do
      args <- many pattern
      return (name, args)
    PApp (PVar name) args -> do
      moreArgs <- many pattern
      return (name, args ++ moreArgs)
    _ -> do
      myTrace "Function definition must start with a variable name"
      empty
