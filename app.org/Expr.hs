{-# LANGUAGE LambdaCase #-}

module Expr (parseExpr) where

-- parser-combinatorsパッケージ
-- import Control.Applicative ((<|>))

-- ← これで Lexer.Token を使う！
-- import Text.Megaparsec.Expr (Operator (..))

-- import Control.Monad.Combinators.Expr

import Control.Applicative (Alternative (..), empty, liftA, liftA2, liftA3, many, optional, some, (<|>))
import Control.Monad (MonadPlus (..), void)
import Data.Void
import Lexer (Token (..))
import Text.Megaparsec (Parsec, lookAhead, notFollowedBy, sepBy1, try, (<?>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- === AST ===
data Pattern
  = PVar String
  | PInt Int
  | PWildcard
  deriving (Show, Eq)

data Expr
  = EVar String
  | EInt Int
  | EBinOp String Expr Expr
  | ELet String Expr Expr
  | EIf Expr Expr Expr
  | ELam String Expr -- ← λ式
  | EApp Expr Expr -- ← 関数適用
  | ECase Expr [(Pattern, Expr)] -- ← 追加！
  deriving (Show, Eq)

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

pattern :: Parser Pattern
pattern =
  (symbol "_" >> return PWildcard)
    <|> (PInt <$> int)
    <|> (PVar <$> ident)

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  alts <- some caseAlt
  return (ECase scrutinee alts)

caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  pat <- pattern
  symbol "->"
  body <- expr
  optional (symbol ";") -- ← ここで使う！
  return (pat, body)

{-}
caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  alts <- some caseAlt
  return (ECase scrutinee alts)

caseAlt = do
  pat <- pattern
  symbol "->"
  body <- expr
  return (pat, body)

caseExpr :: Parser Expr
caseExpr = do
  keyword "case"
  scrutinee <- expr
  keyword "of"
  alts <- sepBy1 caseAlt (symbol ";") -- セミコロンで区切る！
  return (ECase scrutinee alts)

caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  pat <- pattern
  symbol "->"
  body <- expr
  optional (symbol ";") -- ← ここで使う！
  return (pat, body)

caseAlt :: Parser (Pattern, Expr)
caseAlt = do
  pat <- pattern
  symbol "->"
  body <- expr
  return (pat, body)
exprUntilNextAlt :: Parser Expr
exprUntilNextAlt = do
  e <- expr
  _ <- optional $ lookAhead (try (pattern >> symbol "->"))
  return e
-}

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  arg <- ident
  symbol "->"
  body <- expr
  return (ELam arg body)

atom :: Parser Expr
atom =
  parens expr
    <|> (EVar <$> ident)
    <|> (EInt <$> int)

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

expr :: Parser Expr
expr = binOpExpr

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  name <- ident
  symbol "="
  val <- expr -- ここは再帰的に let を許す
  keyword "in"
  body <- binOpExpr -- ここは let を許さないようにする
  return (ELet name val body)

term :: Parser Expr
term =
  (EVar <$> ident)
    <|> (EInt <$> int)
    <|> parens expr

parens :: Parser a -> Parser a
parens p = do
  symbol "("
  x <- p
  symbol ")"
  return x

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
