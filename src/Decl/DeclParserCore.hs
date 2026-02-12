{-# LANGUAGE LambdaCase #-}

module Decl.DeclParserCore where

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

-- decls :: Parser [Decl]
-- decls = many decl
{-}
decl :: Parser Decl
decl = do
  skipNewlines
  myTrace "<< decl parser called"
  declBody
-}
isEOF :: Parser Bool
isEOF = Parser $ \ts ->
  case ts of
    [] -> Just (True, [])
    _ -> Just (False, ts)

decl :: Parser Decl
decl = do
  skipNewlines
  t <- lookAhead anyToken
  myTrace ("<< decl next token: " ++ show t)
  eof <- isEOF
  if eof
    then Parser $ \_ -> Nothing -- many decl に「もう終わり」と伝える
    else do
      myTrace "<< decl parser called"
      declBody

declBody :: Parser Decl
declBody = do
  d <- declDispatch
  myTrace ("<< declBody: return " ++ show d)
  return d

declDispatch :: Parser Decl
declDispatch = do
  t <- lookAhead anyToken
  myTrace ("<< decl dispatch: " ++ show t)
  case t of
    TokKeyword "data" -> dataDecl
    TokKeyword "newType" -> newtypeDecl
    TokKeyword "import" -> importDecl
    TokKeyword "instance" -> instanceDecl
    TokKeyword "module" -> moduleDecl
    TokKeyword "class" -> classDecl
    TokKeyword "type" -> typeAliasDecl
    -- _ -> try funDecl <|> valueDecl
    TokIdent _ -> try typeSigDecl <|> try funDecl <|> valueDecl
    TokSymbol "(" -> try typeSigDecl <|> empty -- "unexpected symbol in declaration"
    _ -> do
      myTrace ("<< unknown token in decl: " ++ show t)
      empty

-- Haskell ファイル全体
program :: Parser [Decl]
program = many decl

{-}
-- 関数宣言
funDecl :: Parser Decl
funDecl = do
  name <- ident
  args <- many patternParser
  t <- lookAhead anyToken
  myTrace ("<< funcdecl 2: " ++ show t)
  symbol "="
  body <- expr
  return (DeclFun (PConstr name args) body)

funDecl :: Parser Decl
funDecl = do
  myTrace "<< funDecl parser called"
  (name, args) <- funHead
  symbol "="
  body <- expr
  myTrace ("<< funDecl return" ++ show body)
  return (DeclFun name args body)
-}

funDecl :: Parser Decl
funDecl = do
  name <- ident
  args <- many patternParser
  myTrace ("<< funcdecl: " ++ show args)
  t <- lookAhead anyToken
  myTrace ("<< decl dispatch: " ++ show t)
  case t of
    TokSymbol "=" -> parseSimple name args
    TokSymbol "{" -> parseGuarded name args
    TokSymbol "|" -> parseGuarded2 name args
    _ -> empty

parseSimple :: Name -> [Pattern] -> Parser Decl
parseSimple name args = do
  symbol "="
  e <- expr
  return (DeclFun name args Nothing (Just e))

parseGuarded :: Name -> [Pattern] -> Parser Decl
parseGuarded name args = do
  symbol "{"
  guards <- guardedRhs
  symbol "}"
  return (DeclFun name args (Just guards) Nothing)

parseGuarded2 :: Name -> [Pattern] -> Parser Decl
parseGuarded2 name args = do
  guards <- guardedRhs2
  return (DeclFun name args (Just guards) Nothing)

guardedRhs2 :: Parser [(Expr, Expr)]
guardedRhs2 = many guardedLine

guardedRhs :: Parser [(Expr, Expr)]
guardedRhs = sepBy1 guardedLine (symbol ";")

guardedLine :: Parser (Expr, Expr)
guardedLine = do
  t <- lookAhead anyToken
  myTrace ("<< guredLine: " ++ show t)
  symbol "|"
  cond <- expr
  symbol "="
  body <- expr
  return (cond, body)

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

{-}
typeSigDecl :: Parser Decl
typeSigDecl = do
  name <- ident
  symbol "::"
  ty <- parseType
  myTrace ("<< parsed type signature: " ++ name ++ " :: " ++ show ty)
  let decl = DeclTypeSig name ty
  myTrace ("<< returning DeclTypeSig: " ++ show decl)
  return decl
-}
typeSigDecl :: Parser Decl
typeSigDecl = do
  name <- ident <|> operator -- name
  symbol "::"
  ty <- parseType
  myTrace ("<< parsed type signature: " ++ name ++ " :: " ++ show ty)
  let decl = DeclTypeSig name ty
  myTrace ("<< returning DeclTypeSig: " ++ show decl)
  return decl

-- 値宣言
valueDecl :: Parser Decl
valueDecl = do
  t <- lookAhead anyToken
  myTrace ("<< valueDecl: " ++ show t)
  pat <- patternParser
  symbol "="
  body <- expr
  return (DeclValue pat body)

-- import 文
importDecl :: Parser Decl
importDecl = do
  myTrace "<< importDecl parser called"
  _ <- keyword "import"
  isQual <- option False (True <$ keyword "qualified")
  t <- lookAhead anyToken
  myTrace ("<< importDecl: " ++ show t)
  modName <- moduleName
  alias <- optional (keyword "as" *> identI)
  isHiding <- option False (True <$ keyword "hiding")
  items <- optional importList
  return $ DeclImport isQual modName alias isHiding items

importList :: Parser [ImportItem]
importList =
  parens $
    pure ImportAllItems <$ symbol ".."
      <|> sepBy1 importIdent (symbol ",")

importIdent :: Parser ImportItem
importIdent = do
  name <- identI
  m <-
    optional $
      parensI $
        (ImportTypeAll name <$ symbol "..")
          <|> (ImportTypeSome name <$> sepBy1 identI (symbol ","))
  return $ case m of
    Just x -> x
    Nothing -> ImportVar name

identI :: Parser String
identI = ident <|> typeIdent

parensI :: Parser a -> Parser a
parensI p = symbol "(" *> p <* symbol ")"

moduleName :: Parser String
moduleName = intercalate "." <$> sepBy1 identI tokdot

tokdot :: Parser String
tokdot = token TokDot *> pure "."

-- data 宣言
dataDecl :: Parser Decl
dataDecl = do
  myTrace "<< dataDecl parser called"
  keyword "data"
  name <- typeIdent
  vars <- many typeIdent
  symbol "="
  constrs <- constr `sepBy1` symbol "|"
  return (DeclData name vars constrs)

-- コンストラクタ
constr :: Parser Constraint
constr = do
  myTrace "<< constr parser called"
  cname <- typeIdent
  tys <- many parseType
  return (Constraint cname tys)

-- newtype 宣言
newtypeDecl :: Parser Decl
newtypeDecl = do
  myTrace "<< newtypeDecl parser called"
  keyword "newtype"
  name <- typeIdent
  vars <- many typeIdent
  symbol "="
  c <- constr
  return (DeclNewtype name vars c)

moduleDecl :: Parser Decl
moduleDecl = do
  keyword "module"
  name <- typeIdent -- <|> token TokTypeIdent)
  keyword "where"
  return (DeclModule name)

instanceDecl :: Parser Decl
instanceDecl = do
  myTrace "<< instanceDecl parser called"
  keyword "instance"
  ctx <- optional (try (constraintList <* keyword "=>"))
  className <- typeIdent
  args <- some typeAtom
  keyword "where"
  methods <- bracedBlock decl
  return (DeclInstance ctx className args methods)

classDecl :: Parser Decl
classDecl = do
  myTrace "<< classDecl parser called"
  keyword "class"
  className <- typeIdent
  vars <- some ident
  keyword "where"
  t <- lookAhead anyToken
  methods <- bracedBlock decl
  return $ DeclClass className vars methods

typeAliasDecl :: Parser Decl
typeAliasDecl = do
  myTrace "<< typeAliasDecl parser called"
  keyword "type"
  name <- typeIdent
  vars <- many ident
  symbol "="
  body <- parseType
  return $ DeclTypeAlias name vars body
