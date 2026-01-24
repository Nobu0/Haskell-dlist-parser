{-# LANGUAGE LambdaCase #-}

module Expr (parseExpr, Expr, Pattern) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (MonadPlus (..))
import Data.Char (isUpper)
-- import Data.Void

import Data.List (isPrefixOf)
import Lexer (Token (..))

-- import Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer as L

-- === 差分リスト風パーサー ===
newtype Parser a = Parser {runParser :: [Token] -> Maybe (a, [Token])}

-- type Parser = Parsec Void [Token]

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

instance Functor Parser where
  fmap f p = Parser $ \input ->
    case runParser p input of
      Just (a, rest) -> Just (f a, rest)
      Nothing -> Nothing

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)
  pf <*> pa = Parser $ \input ->
    case runParser pf input of
      Just (f, rest1) ->
        case runParser pa rest1 of
          Just (a, rest2) -> Just (f a, rest2)
          Nothing -> Nothing
      Nothing -> Nothing

instance Monad Parser where
  p >>= f = Parser $ \input ->
    case runParser p input of
      Just (a, rest) -> runParser (f a) rest
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \input ->
    runParser p1 input <|> runParser p2 input

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> return x

-- === AST ===
data Pattern
  = PVar String
  | PInt Int
  | PWildcard
  | PCons Pattern Pattern
  | PList [Pattern]
  | PTuple [Pattern] -- これが必要
  | PConstr String [Pattern] -- ← これ！
  deriving (Show, Eq)

data Expr
  = EVar String
  | EInt Int
  | EBinOp String Expr Expr
  | ELet [(String, Expr)] Expr
  | EIf Expr Expr Expr
  | ELam String Expr
  | EApp Expr Expr
  | ECase Expr [(Pattern, Expr)]
  | EList [Expr] -- ← 追加！
  | ETuple [Expr] -- ← 追加！
  | ERange Expr Expr
  | EListComp Expr [Qualifier]
  | EAnn Expr Type -- ← 型注釈を表すノード
  deriving (Eq, Show)

--  | ELet String Expr Expr

data Qualifier
  = EGenerator String Expr
  | EGuard Expr
  deriving (Eq, Show)

data Type
  = TVar String -- 型変数（a, b, ...）
  | TCon String -- 型定数（Int, Bool, ...）
  | TArrow Type Type -- 関数型（a -> b）
  | TList Type -- リスト型 [a]
  | TApp Type Type
  | TConstraint [Constraint] Type -- ★ 型制約付きの型
  | TForall [String] Type
  deriving (Eq, Show)

data Constraint = Constraint String [Type]
  deriving (Eq, Show)

parseType :: Parser Type
parseType = parseForall <|> parseArrowType

parseForall :: Parser Type
parseForall = do
  token TokForall
  vars <- some ident
  token TokDot
  body <- parseType
  return $ TForall vars body

parseArrowType :: Parser Type
parseArrowType = do
  t1 <- parseAtomicType
  rest t1
  where
    rest t1 =
      ( do
          token TokArrow
          t2 <- parseArrowType
          return $ TArrow t1 t2
      )
        <|> return t1

parseAtomicType :: Parser Type
parseAtomicType =
  (TVar <$> ident)
    <|> do
      token TokLParen
      t <- parseType
      token TokRParen
      return t

constraintP :: Parser Constraint
constraintP = do
  className <- typeIdent
  args <- some typeAtom
  return $ Constraint className args

constraintList :: Parser [Constraint]
constraintList =
  try (parens (constraintP `sepBy1` symbol ",")) -- 複数制約
    <|> fmap (: []) constraintP -- 単一制約

typeP :: Parser Type
typeP = do
  constraints <- optional $ try $ do
    cs <- constraintList
    symbol "=>"
    return cs
  t <- arrowType
  return $ maybe t (`TConstraint` t) constraints

typeApp :: Parser Type
typeApp = do
  f <- typeAtom
  args <- many typeAtom
  return $ foldl TApp f args

arrowType :: Parser Type
arrowType = do
  t1 <- typeApp
  rest <- optional (token TokArrow *> arrowType)
  -- rest <- optional (symbol "->" *> arrowType)
  return $ maybe t1 (TArrow t1) rest

typeIdent :: Parser String
typeIdent = satisfyMap $ \case
  TokTypeIdent s -> Just s
  _ -> Nothing

typeAtom =
  (TCon <$> typeIdent)
    <|> (TVar <$> ident)
    <|> brackets (TList <$> typeP)
    <|> parens typeP

isTypeIdent :: Token -> Bool
isTypeIdent (TokIdent (c : _)) = isUpper c
isTypeIdent _ = False

satisfyMap :: (Token -> Maybe a) -> Parser a
satisfyMap f = Parser $ \tokens -> case tokens of
  (t : ts) -> case f t of
    Just x -> Just (x, ts)
    Nothing -> Nothing
  [] -> Nothing

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = Parser $ \tokens -> case tokens of
  (t : ts) | f t -> Just (t, ts)
  _ -> Nothing

tokens :: [Token] -> Parser [Token]
tokens expected = Parser $ \input ->
  if expected `isPrefixOf` input
    then Just (expected, drop (length expected) input)
    else Nothing

token :: Token -> Parser Token
token t = satisfy (== t)

-- elist :: Parser Expr
-- elist = brackets (try range <|> list)
elist :: Parser Expr
elist = brackets (try listComp <|> try range <|> list)

range :: Parser Expr
range = do
  start <- expr
  symbol ".."
  end <- expr
  return (ERange start end)

list :: Parser Expr
list = do
  elems <- expr `sepBy` symbol ","
  return (EList elems)

listComp :: Parser Expr
listComp = do
  body <- expr
  symbol "|"
  qualifiers <- qualifier `sepBy` symbol ","
  return (EListComp body qualifiers)

qualifier :: Parser Qualifier
qualifier =
  try generator
    <|> guardExpr

generator :: Parser Qualifier
generator = do
  var <- ident
  symbol "<-"
  val <- expr
  return (EGenerator var val)

guardExpr :: Parser Qualifier
guardExpr = EGuard <$> expr

-- tryの実装
-- 失敗しても入力を消費しなかったかのように振る舞う
try :: Parser a -> Parser a
try p = Parser $ \tokens ->
  runParser p tokens

pAtom :: Parser Pattern
pAtom =
  pWildcard
    <|> pList
    <|> pParenOrTuple
    <|> pConstrOrVar
    <|> pInt

pConstrOrVar :: Parser Pattern
pConstrOrVar = do
  name <- ident
  args <- many pAtom
  if isUpper (head name)
    then return (PConstr name args)
    else case args of
      [] -> return (PVar name)
      _ -> error $ "変数 " ++ name ++ " に引数がついています"

pParenOrTuple :: Parser Pattern
pParenOrTuple = parens $ do
  pats <- pattern `sepBy1` symbol ","
  return $ case pats of
    [single] -> single
    _ -> PTuple pats

pattern :: Parser Pattern
pattern = makeCons

makeCons :: Parser Pattern
makeCons = do
  hd <- pAtom
  rest <- optional (symbol ":" *> pattern)
  case rest of
    Just tl -> return (PCons hd tl)
    Nothing -> return hd

-- pVar :: Parser Pattern
-- pVar = PVar <$> ident

pInt :: Parser Pattern
pInt = PInt <$> int

pWildcard :: Parser Pattern
pWildcard = symbol "_" >> return PWildcard

pList :: Parser Pattern
pList = PList <$> brackets (pattern `sepBy` symbol ",")

-- pList = PList <$> brackets (pCons `sepBy` symbol ",")

-- 指定したパーサーを、さらに特定の記号で囲む補助関数
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

-- 括弧 () で囲む
parens :: Parser a -> Parser a
parens p = between (symbol "(") (symbol ")") p

-- 角括弧 [] で囲む
brackets :: Parser a -> Parser a
brackets p = between (symbol "[") (symbol "]") p

-- 0回以上の出現を解析（手動再帰版）
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
  ( do
      x <- p
      xs <- many (sep *> p)
      return (x : xs)
  )
    <|> pure []

-- 1回以上の出現を解析（手動再帰版）
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  first <- p
  rest <- (sep *> sepBy1 p sep) <|> pure [] -- ここがポイント
  return (first : rest)

-- 1回以上の出現を解析（末尾の区切り記号を許容する）
sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do
  x <- p
  ( do
      _ <- sep
      -- 区切り記号の後にさらに要素が続くか試す
      xs <- sepEndBy p sep
      return (x : xs)
    )
    <|> pure [x] -- 区切り記号がない、または末尾だった場合は [x] を返す

-- 0回以上の出現を解析（末尾の区切り記号を許容する）
sepEndBy :: Parser a -> Parser sep -> Parser [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  alts <- bracesCase <|> plainCase
  return (ECase scrutinee alts)

bracesCase :: Parser [(Pattern, Expr)]
bracesCase = braces (caseAlt `sepEndBy1` symbol ";")

plainCase :: Parser [(Pattern, Expr)]
plainCase = some caseAlt

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  pat <- pattern
  token TokArrow
  -- symbol "->"
  body <- expr
  _ <- optional (symbol ";") -- ← ここで使う！
  return (pat, body)

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  arg <- ident
  token TokArrow
  -- symbol "->"
  body <- expr
  return (ELam arg body)

atom :: Parser Expr
atom =
  parens exprOrTuple
    <|> try elist
    <|> (EVar <$> ident)
    <|> (EInt <$> int)

-- <|> listExpr

-- タプルまたは括弧式
exprOrTuple :: Parser Expr
exprOrTuple = do
  es <- expr `sepBy1` symbol ","
  return $ case es of
    [single] -> single
    _ -> ETuple es

appExpr :: Parser Expr
appExpr = do
  f <- atom
  args <- many atom
  return (foldl EApp f args)

binOpExpr :: Parser Expr
binOpExpr = exprLevel1

exprLevel1 :: Parser Expr
exprLevel1 = chainl1 exprLevel2 (binOp ["+", "-"])

exprLevel2 :: Parser Expr
exprLevel2 = chainl1 exprLevel3 (binOp ["*", "/"])

exprLevel3 :: Parser Expr
exprLevel3 =
  lambdaExpr
    <|> letExpr
    <|> ifExpr
    <|> caseExpr -- ← ここ！
    <|> appExpr

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \case
  TokOperator op | op `elem` ops -> Just (EBinOp op)
  _ -> Nothing

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  cond <- expr
  keyword "then"
  thenBranch <- expr
  keyword "else"
  elseBranch <- expr
  return (EIf cond thenBranch elseBranch)

tokenIs :: (Token -> Maybe a) -> Parser a
tokenIs f = Parser $ \case
  (t : ts) -> case f t of
    Just a -> Just (a, ts)
    Nothing -> Nothing
  [] -> Nothing

-- === トークンパーサー ===

ident :: Parser String
ident = tokenIs (\case TokIdent s -> Just s; _ -> Nothing)

int :: Parser Int
int = tokenIs (\case TokNumber n -> Just n; _ -> Nothing)

keyword :: String -> Parser ()
keyword kw = tokenIs (\case TokKeyword s | s == kw -> Just (); _ -> Nothing)

symbol :: String -> Parser ()
symbol s = tokenIs (\case TokSymbol s' | s == s' -> Just (); _ -> Nothing)

-- === 構文解析 ===

expr = do
  e <- binOpExpr
  mt <- optional (symbol "::" *> typeP)
  return $ maybe e (EAnn e) mt

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  defs <- def `sepBy1` symbol ";"
  keyword "in"
  body <- expr
  return (ELet defs body)

def :: Parser (String, Expr)
def = do
  name <- ident
  symbol "="
  val <- expr
  return (name, val)

-- === 実行関数 ===
parseExpr :: [Token] -> IO (Maybe Expr)
parseExpr toks = do
  -- putStrLn $ "Tokens: " ++ show toks -- ← 追加！
  case runParser expr toks of
    Just (e, rest) -> do
      putStrLn $ "Parsed expression: " ++ show e
      putStrLn $ "Remaining tokens: " ++ show rest
      return (Just e)
    Nothing -> do
      putStrLn "Parsing failed!"
      return Nothing
