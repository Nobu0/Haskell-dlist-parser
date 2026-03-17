{-# LANGUAGE LambdaCase #-}

module Parser.Core.TokenParser
  ( ident,
    int,
    keyword,
    symbol,
    tokenIs,
    parens,
    parensOpt,
    brackets,
    braces,
    bracesv,
    bracesVO,
    bracesV,
    bracesB,
    -- bracesN,
    skipVB,
    newline,
    notFollowedBy,
    (<?>),
    anyToken,
    stringLiteralExpr,
    charLiteralExpr,
    skipNewlines,
    newline,
    skipSeparators,
    -- skipVNl,
    skipNL,
    skipVNL,
    aliasName,
    qualifiedIdent,
    -- skipVLBrace,
    name,
    operator,
    bracedBlock,
    anyToken,
    binOp,
    parseBinOp,
    -- operatorVar,
    satisfyToken,
    symbolToken,
    operatorI,
    -- operatorA,
    -- operatorB,
    operatorIAsName,
    operatorEdName,
    identI,
    typeIdent,
    isSymbolName,
    isEOF,
    eof,
  )
where

import AST.Expr
import Control.Applicative (empty, many, optional, (<|>))
import Data.Char (isAlpha)
import Data.Functor (void)
import Data.List (intercalate)
import qualified Data.Set as Set
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Utils.MyTrace (myTrace)

-- import Text.Megaparsec (token, (<?>))

(<?>) :: Parser a -> String -> Parser a
p <?> _ = p

identI :: Parser String
identI = ident <|> typeIdent

braces :: Parser a -> Parser a
braces p = between (symbol "{") (symbol "}") p

bracesv :: Parser a -> Parser a
bracesv p = do
  -- optional (symbol ";")
  between (token TokVLBrace) (token TokVRBrace) p

bracesVO :: Parser a -> Parser a
bracesVO p = try (bracesv p) <|> p

bracesB :: Parser a -> Parser a
bracesB p = try (braces p) <|> p

bracesV :: Parser b -> Parser b
bracesV p = do
  mtok <- optional (lookAhead anyToken)
  case mtok of
    Just TokVLBrace -> do
      token TokVLBrace
      x <- p
      optional (token TokNewline)
      token TokVRBrace
      return x
    Just (TokSymbol "{") -> braces p
    _ -> p

parens :: Parser a -> Parser a
parens p = do
  symbol "("
  x <- p
  optional (symbol ";")
  symbol ")"
  return x

parensOpt :: Parser a -> Parser a
parensOpt p = do
  mtok <- optional (lookAhead anyToken)
  case mtok of
    Just (TokSymbol "(") -> parens p
    _ -> p

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
    TokNumber n -> pure n
    _ -> empty
  where
    isNumber (TokNumber _) = True
    isNumber _ = False

keyword :: String -> Parser ()
keyword kw = do
  t <- satisfy isKeyword
  case t of
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

isSymbolName :: String -> Bool
isSymbolName s =
  case s of
    ('(' : _) | last s == ')' -> True -- 括弧付き演算子 (<?>)
    _ -> not (null s) && isSymbolStart (head s)

isSymbolStart :: Char -> Bool
isSymbolStart c = c `elem` "!#$%&*+./<=>?@\\^|-~:"

aliasName :: Parser String
aliasName = intercalate "." <$> sepBy1 typeIdent (token (TokOperator "."))

qualifiedIdent :: Parser String
qualifiedIdent = intercalate "." <$> sepBy1 identI (token (TokOperator "."))

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
peekToken = Parser $ \tokens ->
  case tokens of
    [] -> Nothing
    (t : _) -> Just (t, tokens)

typeIdent :: Parser String
typeIdent = satisfyMap $ \case
  TokTypeIdent s -> Just s
  _ -> Nothing

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
satisfyToken f = Parser $ \ts ->
  case ts of
    [] -> Nothing
    (t : ts') -> case f t of
      Just x -> Just (x, ts')
      Nothing -> Nothing

skipVNl :: Parser ()
skipVNl = do
  _ <- tokenIs isSep
  return ()
  where
    isSep TokVNl = Just ()
    isSep _ = Nothing

skipVNL :: Parser ()
skipVNL = do
  _ <- many $ tokenIs isSep
  return ()
  where
    isSep (TokNewline) = Just ()
    isSep (TokSymbol ";") = Just ()
    isSep _ = Nothing

skipNL :: Parser ()
skipNL = do
  _ <- many $ tokenIs isSep
  return ()
  where
    isSep (TokSymbol ";") = Just ()
    isSep _ = Nothing

skipSeparators :: Parser ()
skipSeparators = do
  _ <- many (tokenIs isSep)
  return ()
  where
    isSep TokVNl = Just ()
    isSep TokNewline = Just ()
    isSep (TokSymbol ";") = Just ()
    isSep _ = Nothing

skipVB :: Parser ()
skipVB = do
  _ <- many (tokenIs isSep)
  return ()
  where
    isSep TokVLBrace = Just ()
    isSep TokVRBrace = Just ()
    isSep _ = Nothing

newline :: Parser ()
newline = void (token TokNewline)

skipNewlines :: Parser ()
skipNewlines = do
  _ <- many (tokenIs (\t -> if t == TokNewline then Just () else Nothing))
  return ()

binOp :: [String] -> Parser (Expr -> Expr -> Expr)
binOp ops = tokenIs $ \tok ->
  case tok of
    TokOperator s | s `elem` ops ->
      case parseBinOp s of
        Just bop -> Just (\a b -> EBinOp bop a b)
        Nothing -> Nothing
    _ -> Nothing

parseBinOp :: String -> Maybe BinOp
parseBinOp s = case s of
  -- 算術演算
  "+" -> Just BinOpAdd
  "-" -> Just BinOpSub
  "*" -> Just BinOpMul
  "/" -> Just BinOpDiv
  -- 比較演算
  "==" -> Just BinOpEq
  "/=" -> Just BinOpNeq
  "!=" -> Just BinOpNeq
  "<" -> Just BinOpLt
  ">" -> Just BinOpGt
  "<=" -> Just BinOpLe
  ">=" -> Just BinOpGe
  -- 論理演算
  "&&" -> Just BinOpAnd
  "||" -> Just BinOpOr
  -- リスト・文字列操作
  "++" -> Just BinOpConcat
  ":" -> Just BinOpCons
  -- 関数合成
  "." -> Just BinOpCompose
  -- モナディック操作など（必要に応じて）
  "*>" -> Just BinOpThen
  "<*" -> Just BinOpThen
  "<?>" -> Just BinOpThen
  "<$" -> Just BinOpThen
  "<*>" -> Just BinOpThen
  "<+>" -> Just BinOpThen
  ">>" -> Just BinOpThen
  ">>=" -> Just BinOpBind
  "<|>" -> Just BinOpAlt
  "<$>" -> Just BinOpFmap
  "<>" -> Just BinOpAppend
  "$" -> Just BinOpApp
  "\\" -> Just BinOpListDiff
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

-- importに関係する
operatorI :: Parser String
operatorI = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` allowed = Just s
      | otherwise = Nothing
    isOp _ = Nothing
    allowed = ["::", ":", "++", "<$>", "\\", "<*>", ">>=", "<|>", "<?>", "<+>"] -- "$" を含めない！

operatorAll :: Parser String
operatorAll = satisfyToken f
  where
    f (TokOperator s) = Just s
    f _ = Nothing

operatorEdName :: Parser Name
operatorEdName = do
  symbol "("
  op <- operatorAll
  symbol ")"
  return $ "(" ++ op ++ ")"

operatorIAsName :: Parser Name
operatorIAsName = do
  op <-
    try operatorEdName -- 括弧付き演算子（例: (<?>), (++)
      <|> operatorI -- 括弧なし演算子（例: <?>, ++）
  myTrace ("<< operatorIAsName: " ++ show op)
  return op

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

isEOF :: Parser Bool
isEOF = Parser $ \ts ->
  case ts of
    [] -> Just (True, [])
    _ -> Just (False, ts)

eof :: Parser ()
eof = Parser $ \ts ->
  case ts of
    [] -> Just ((), [])
    _ -> Nothing
