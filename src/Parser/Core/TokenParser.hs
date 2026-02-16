{-# LANGUAGE LambdaCase #-}

module Parser.Core.TokenParser
  ( ident,
    int,
    keyword,
    symbol,
    tokenIs,
    parens,
    brackets,
    braces,
    bracesv,
    bracesV,
    bracesV3,
    notFollowedBy,
    (<?>),
    anyToken,
    stringLiteralExpr,
    charLiteralExpr,
    skipNewlines,
    newline,
    skipSeparators,
    name,
    operator,
    bracedBlock,
    anyToken,
    binOp,
    operatorVar,
    satisfyToken,
    symbolToken,
  )
where

import AST.Expr
import Control.Applicative (empty, many, (<|>))
import Data.Functor (void)
import qualified Data.Set as Set
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Utils.MyTrace (myTrace)

-- import Text.Megaparsec (token, (<?>))

(<?>) :: Parser a -> String -> Parser a
p <?> _ = p

braces :: Parser a -> Parser a
braces p = between (symbol "{") (symbol "}") p

bracesv :: Parser a -> Parser a
bracesv p = between (token TokVLBrace) (token TokVRBrace) p

-- bracesV :: Parser a -> Parser a
-- bracesV p = between (token TokVLBrace) (token TokVRBrace) p

-- 仮想括弧
bracesV :: Parser a -> Parser a
bracesV p = try (bracesv p) <|> (braces p)

-- 括弧無しでも扱う
bracesV3 :: Parser a -> Parser a
bracesV3 p = do try (bracesv p) <|> (braces p) <|> p

-- parens :: Parser a -> Parser a
-- parens p = between (symbol "(") (symbol ")") p
parens :: Parser a -> Parser a
parens p = do
  symbol "("
  x <- p
  symbol ")"
  return x

brackets :: Parser a -> Parser a
brackets p = between (symbol "[") (symbol "]") p

ident :: Parser String
ident = tokenIs $ \case
  TokIdent s -> Just s
  _ -> Nothing

int :: Parser Int
int = do
  t <- satisfy isNumber
  case t of
    -- TokNumber n -> myTrace ("<< int: " ++ show n) >> pure n
    TokNumber n -> pure n
    -- TokNumber n -> pure n
    _ -> empty
  where
    isNumber (TokNumber _) = True
    isNumber _ = False

keyword :: String -> Parser ()
keyword kw = do
  t <- satisfy isKeyword
  case t of
    -- TokKeyword s | s == kw -> myTrace ("<< keyword: " ++ s) >> pure ()
    TokKeyword s | s == kw -> pure ()
    _ -> empty
  where
    isKeyword (TokKeyword _) = True
    isKeyword _ = False

symbol :: String -> Parser ()
symbol s = tokenIs $ \case
  TokSymbol s' | s' == s -> Just ()
  TokOperator s' | s' == s -> Just ()
  _ -> Nothing

tokenIs :: (Token -> Maybe a) -> Parser a
tokenIs f = Parser $ \case
  (t : ts) -> case f t of
    Just a -> Just (a, ts)
    Nothing -> Nothing
  [] -> Nothing

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \input ->
  case runParser p input of
    Nothing -> Just ((), input) -- p が失敗 → 成功
    Just _ -> Nothing -- p が成功 → 失敗

anyToken :: Parser Token
anyToken = Parser $ \input ->
  case input of
    (t : ts) -> Just (t, ts)
    [] -> Nothing

debugPeek :: Parser ()
debugPeek = do
  t <- peekToken
  Parser $ \tokens ->
    Just ((), tokens)

peekToken :: Parser Token
peekToken = Parser $ \tokens -> case tokens of
  [] -> Nothing
  (t : _) -> Just (t, tokens)

stringLiteralExpr :: Parser String
stringLiteralExpr =
  satisfyToken f
  where
    f (TokString s) = Just s
    f _ = Nothing

charLiteralExpr :: Parser Char
charLiteralExpr =
  satisfyToken f
  where
    f (TokChar s) = Just s
    f _ = Nothing

satisfyToken :: (Token -> Maybe a) -> Parser a
satisfyToken f = Parser $ \ts -> case ts of
  [] -> Nothing
  (t : ts') -> case f t of
    Just x -> Just (x, ts')
    Nothing -> Nothing

skipSeparators :: Parser ()
skipSeparators = do
  _ <- many (tokenIs isSep)
  return ()
  where
    isSep TokNewline = Just ()
    isSep (TokSymbol ";") = Just ()
    isSep _ = Nothing

newline :: Parser ()
newline = void (token TokNewline)

skipNewlines :: Parser ()
skipNewlines = do
  _ <- many (tokenIs (\t -> if t == TokNewline then Just () else Nothing))
  return ()

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \case
  TokOperator op | op `elem` ops ->
    case parseBinOp op of
      Just bop -> Just (EBinOp bop)
      Nothing -> Nothing
  _ -> Nothing

parseBinOp :: String -> Maybe BinOp
parseBinOp s = case s of
  "+" -> Just Add
  "-" -> Just Sub
  "*" -> Just Mul
  "/" -> Just Div
  "==" -> Just Eq
  "!=" -> Just Neq
  "<" -> Just Lt
  ">" -> Just Gt
  "<=" -> Just Le
  ">=" -> Just Ge
  "&&" -> Just And
  "||" -> Just Or
  "++" -> Just Concat
  ":" -> Just Cons
  _ -> Nothing

operator :: Parser String
operator = choice (map (\s -> symbol s >> return s) allOps)
  where
    allOps =
      [ "==",
        "/=",
        ">=",
        "<=",
        "+",
        "-",
        "*",
        "/",
        ">",
        "<",
        ":"
      ]

operatorVar :: Parser Expr
operatorVar = do
  op <- satisfyToken isOp
  return (EVar op)
  where
    isOp (TokOperator s) = Just s
    isOp _ = Nothing

{-}
operator :: Parser String
operator = do
  symbol "("
  op <- operatorTok
  symbol ")"
  return (op)
-}

-- 関数名や演算子名をパースする共通パーサー
-- 例: "f" や "==" や "(==)"
name :: Parser String
name = try parenOp <|> ident <|> symbolOp

-- 括弧付きオペレータ: (==)
parenOp :: Parser String
parenOp = do
  symbol "("
  op <- symbolOp
  symbol ")"
  return op

-- 括弧なしのオペレータ: ==
symbolOp :: Parser String
symbolOp = do
  tok <- satisfy isSymbol
  case tok of
    TokSymbol s -> return s
    _ -> empty -- ここには来ないはずだけど、安全のため

-- 文字列がオペレータかどうか
isSymbol :: Token -> Bool
isSymbol (TokSymbol _) = True
isSymbol _ = False

bracedBlock :: Parser a -> Parser [a]
bracedBlock p = do
  symbol "{"
  go 1 []
  where
    go 0 acc = return (reverse acc)
    go n acc = do
      t <- lookAhead anyToken
      case t of
        TokSymbol "{" -> symbol "{" >> go (n + 1) acc
        TokSymbol "}" -> symbol "}" >> go (n - 1) acc
        _ -> do
          x <- p
          go n (x : acc)

symbolToken :: Token -> Parser Token
symbolToken tok = satisfyToken match
  where
    match t
      | t == tok = Just t
      | otherwise = Nothing
