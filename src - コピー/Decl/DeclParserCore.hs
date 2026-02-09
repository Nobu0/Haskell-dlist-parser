{-# LANGUAGE LambdaCase #-}

module Decl.DeclParserCore where

import AST.Decl
import AST.Module (Name)
import AST.Pattern (Pattern (..))
import AST.Type (Type (..))
import Control.Applicative (empty, many, optional, some, (<|>))
import Data.List (intercalate)
-- ★ ここが正しい

-- (keyword) -- , whereClause)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.ExprExtensions (expr, skipNewlines)
import Parser.Expr.PatternParser (pattern, patternParser)
import Parser.Type.TypeParser (parseType, typeIdent, typeP)
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
  eof <- isEOF
  if eof
    then Parser $ \_ -> Nothing -- many decl に「もう終わり」と伝える
    else do
      myTrace "<< decl parser called"
      declBody

declBody :: Parser Decl
declBody = do
  declDispatch
  where
    declDispatch = do
      t <- lookAhead anyToken
      myTrace ("<< decl dispatch: " ++ show t)
      case t of
        TokKeyword "data" -> dataDecl
        TokKeyword "newType" -> newtypeDecl
        TokKeyword "import" -> importDecl
        TokKeyword "module" -> moduleDecl
        -- _ -> try funDecl <|> valueDecl
        TokIdent _ -> try typeSigDecl <|> funDecl <|> valueDecl

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
-}

funDecl :: Parser Decl
funDecl = do
  myTrace "<< funDecl parser called"
  (name, args) <- funHead
  symbol "="
  body <- expr
  return (DeclFun name args body)

funHead :: Parser (Name, [Pattern])
funHead = do
  p <- pattern
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

typeSigDecl :: Parser Decl
typeSigDecl = do
  name <- ident
  symbol "::"
  ty <- typeP
  return (DeclTypeSig name ty)

{-}
typeSigDecl :: Parser Decl
typeSigDecl = do
  name <- ident
  symbol "::"
  ty <- typeParser
  return (DeclTypeSig name ty)

typeParser :: Parser Type
typeParser = typeForall

typeForall :: Parser Type
typeForall =
  try
    ( do
        keyword "forall"
        vars <- many1 ident
        symbol "."
        TForall vars <$> typeConstraint
    )
    <|> typeConstraint

typeConstraint :: Parser Type
typeConstraint =
  try
    ( do
        cs <- constraint `sepBy1` symbol ","
        symbol "=>"
        TConstraint cs <$> typeArrow
    )
    <|> typeArrow

typeArrow :: Parser Type
typeArrow =
  do
    t1 <- typeApp
    (symbol "->" >> TArrow t1 <$> typeArrow)
      <|> return t1

typeApp :: Parser Type
typeApp =
  do
    ts <- some typeAtom
    return (foldl1 TApp ts)

typeAtom :: Parser Type
typeAtom =
  TVar <$> ident
    <|> TCon <$> typeIdent
    <|> TList <$> brackets typeParser
    <|> parens (typeParser `sepBy` symbol ",")

constraint :: Parser Constraint
constraint = do
  cls <- typeIdent -- "Eq", "Ord", "Show" など
  tys <- many typeAtom -- 引数
  return (Constraint cls tys)
-}

-- 値宣言
valueDecl :: Parser Decl
valueDecl = do
  myTrace "<< valueDecl parser called"
  pat <- patternParser
  symbol "="
  body <- expr
  return (DeclValue pat body)

-- import 文
importDecl :: Parser Decl
importDecl = do
  myTrace "<< importDecl parser called"
  keyword "import"
  modName <- moduleName
  return (DeclImport modName)

moduleName :: Parser Name
moduleName = intercalate "." <$> sepBy1 ident (symbol ".")

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
constr :: Parser Constr
constr = do
  myTrace "<< constr parser called"
  cname <- typeIdent
  tys <- many parseType
  return (Constr cname tys)

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
