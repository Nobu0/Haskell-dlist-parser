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
  skipSeparators
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< funDecl: next token=" ++ show t ++ " ct=" ++ show ct)
  (name, clause1) <- funClause -- funDecl
  rest <- many (try (funClauseWithName name))
  -- optional (token TokVRBrace)
  myTrace (">>*funDecl name " ++ show name ++ " clause1 " ++ show clause1 ++ " rest " ++ show rest)
  return (DeclFunGroup name (clause1 : rest))

{-}
funClause :: Parser (Name, FunClause)
funClause = do
  ct <- getRemainingCount
  t0 <- lookAhead anyToken
  myTrace ("<< funClause: next token=" ++ show t0 ++ " ct=" ++ show ct)
  -- name <- ident
  pat <- many pPattern -- <|> (many pattern) -- patternParser
  let (name : args) = pat
  skipNL
  t <- lookAhead anyToken
  myTrace ("<< funClause: args(pa)=" ++ show args ++ " t = " ++ show t)
  case t of
    TokSymbol "=" -> parseSimpleClause name args
    TokSymbol "|" -> parseGuardedClause name args
    _ -> parseGuardedClause name args
-}

funClause :: Parser (Name, FunClause)
funClause = do
  pats <- some pPattern -- 関数名も含めてすべてのパターンを取得
  let (namePat : args) = pats
  name <- case namePat of
    PVar n -> return n
    PInfix lhs op rhs ->
      case lhs of
        PVar n -> return n -- 中置パターンの左辺が関数名
        _ -> empty
    _ -> empty
  skipNL
  t <- lookAhead anyToken
  case t of
    TokSymbol "=" -> parseSimpleClause name args
    TokSymbol "|" -> parseGuardedClause name args
    _ -> parseGuardedClause name args

parseSimpleClause :: Name -> [Pattern] -> Parser (Name, FunClause)
parseSimpleClause name args = do
  symbol "="
  skipNL
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< parseSimpleClause: next token=" ++ show t ++ " ct=" ++ show ct)
  e <- exprBlock
  ct <- getRemainingCount
  myTrace ("<< parseSimpleClause: e " ++ show e ++ " ct=" ++ show ct)
  w <- optional whereBlock
  return (name, mkSimpleClause args e w)

parseGuardedClause :: Name -> [Pattern] -> Parser (Name, FunClause)
parseGuardedClause name args = do
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< parseGuardedClause: next token=" ++ show t ++ " ct=" ++ show ct)
  guards <- guardedRhs
  w <- optional (whereBlock)
  return (name, mkGuardedClause args guards w)

funDeclGroup :: Parser Decl
funDeclGroup = do
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< funDeclGroup: next token=" ++ show t ++ " ct=" ++ show ct)
  (name1, clause1) <- funClause
  rest <- many (try (funClauseWithName name1))
  return (DeclFunGroup name1 (clause1 : rest))

exprBlock = do
  expr

-- 同じ名前の関数をグループ化する
funClauseWithName :: Name -> Parser FunClause
funClauseWithName name = do
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< funClauseWithName: next token=" ++ show t ++ " ct=" ++ show ct)
  name' <- ident
  guard (name == name')
  args <- many pPattern -- patternParser
  skipNL
  t <- lookAhead anyToken
  myTrace ("<< funClauseName: args " ++ show args ++ " t " ++ show t)
  case t of
    TokSymbol "=" -> do
      symbol "="
      skipNL
      e <- exprBlock
      w <- optional whereBlock
      return (mkSimpleClause args e w)
    TokSymbol "|" -> do
      guards <- guardedRhs
      w <- optional whereBlock
      return (mkGuardedClause args guards w)
    _ -> do
      e <- exprBlock
      w <- optional whereBlock
      return (mkSimpleClause args e w)

whereBlock :: Parser [Decl]
whereBlock = do
  skipVNL
  keyword "where"
  bracesV $ do
    ct <- getRemainingCount
    t <- lookAhead anyToken
    myTrace ("<< whereBlock(decl): next token " ++ show t ++ " ct=" ++ show ct)
    decls <- many1 funDecl
    return decls

guardedRhsM :: Parser [(Expr, Expr)]
guardedRhsM = do
  ct <- getRemainingCount
  xs <- many1 parseGuardLine
  return xs

parseGuardLine :: Parser (Expr, Expr)
parseGuardLine = do
  skipNL
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< parseGuardLine: next token=" ++ show t ++ " ct=" ++ show ct)
  symbol "|"
  cond <- exprBlock
  skipNL
  symbol "="
  skipNL
  body <- exprBlock
  return (cond, body)

guardedRhs :: Parser [(Expr, Expr)]
guardedRhs = do
  many1 $ do
    skipNL
    ct <- getRemainingCount
    t <- lookAhead anyToken
    e <- parseGuardLine
    myTrace ("<< guardedRhs: next token = " ++ show t ++ " e " ++ show e ++ " ct=" ++ show ct)
    return e

{-}
funHead :: Parser (Name, [Pattern])
funHead = do
  skipNL
  ct <- getRemainingCount
  p <- pattern
  skipNL
  myTrace ("<< funHead pattern: " ++ show p ++ " ct=" ++ show ct)
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
-}
