{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ExprExtensions
  ( expr,
    exprTop,
    exprCore,
    exprNoLoop,
    letExpr,
    ifExpr,
    returnExpr,
    forExpr,
    postfix,
    skipNewlines,
  )
where

import AST.Expr
-- import AST.Decl
import AST.Pattern
import Control.Applicative (empty, many, optional, some, (<|>))
-- (keyword) -- , whereClause)
import Data.Functor (void)
-- import Decl.DeclParserCore (isEOF)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.CaseParserCore (caseExprCore, lambdaCaseExpr)
import Parser.Expr.DoParserCore (doExprCore)
import Parser.Expr.ExprCore
import Parser.Expr.ListParserCore (listExprCore)
import Parser.Expr.PatternParser (pPattern, pattern)
import Parser.SQL.SQLParser
import Parser.Type.TypeParser
import Utils.MyTrace

-- ============================================
--  exprTop / exprSeq
-- ============================================

exprTop :: Parser Expr
exprTop = do
  es <- sepEndBy1 expr exprSep
  return $ if length es == 1 then head es else ESeq es

exprSep :: Parser ()
exprSep = skipMany (symbol ";" <|> newline)

expr :: Parser Expr
expr = do
  e <- infixExpr
  rest <- optional $ do
    symbol "::"
    ty <- typeAtom -- parseType
    return ty
  myTrace ("<< expr: e " ++ show e ++ " rest " ++ show rest)
  case rest of
    Just ty -> return (EAnn e ty)
    Nothing -> return e

infixExpr :: Parser Expr
infixExpr = do
  e <- exprTerm
  -- ct <- getRemainingCount
  -- myTrace ("<< infixExpr: " ++ show e ++ " ct=" ++ show ct)
  -- ここで先読みすると全部失敗するようになる。
  rest <-
    many
      ( do
          skipNL
          op <- infixOp
          e2 <- exprTerm
          return (op, e2)
      )
  return (foldr (\(op, e2) acc -> op acc e2) e rest)

exprTerm :: Parser Expr
exprTerm = do
  f <- exprNoLoop
  args <- many exprNoLoop
  postfix (foldl EApp f args)

exprNoLoop :: Parser Expr
exprNoLoop = do
  e <- exprDispatch
  myTrace ("<< exprNoLoop: e " ++ show e)
  return e

infixOp :: Parser (Expr -> Expr -> Expr)
infixOp = do
  op <- optional operatorB
  myTrace (">> infixOp: out ")
  case op of
    Just mop ->
      case parseBinOp mop of
        Just bop -> do
          myTrace ("<< infixOp: parsed as " ++ show bop)
          return (\a b -> EBinOp bop a b)
        Nothing -> do
          myTrace ("<< infixOp: parseBinOp failed for " ++ show mop)
          empty
    Nothing -> do
      empty

postfix :: Expr -> Parser Expr
postfix e = do
  mop <- optional operatorA
  myTrace ("<< postfix: operator = " ++ show mop)
  case mop of
    Just "$" -> do
      skipNL
      myTrace "<< postfix: operator = $"
      rhs <- expr -- layoutExpr -- ← ここがポイント！
      myTrace (">> postfix: out rhs = " ++ show rhs)
      postfix (EApp (EApp (EVar "$") e) rhs)
    Just op -> do
      myTrace ("<< postfix: infix operator = " ++ show op)
      rhs <- expr -- NoLoop
      postfix (EApp (EApp (EVar op) e) rhs)
    Nothing -> do
      -- skipSeparators
      mbBinds <- whereBlock -- whereClause
      case mbBinds of
        Just binds -> postfix (EWhere e binds)
        Nothing -> return e

-- postfixで参照
operatorA :: Parser String
operatorA = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` ["<$>", "..", ":", "$"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

-- infixで参照
operatorB :: Parser String
operatorB = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` [".", ">>", "++", "<?>", ">>=", "*>", "<$", "<*>", "<*", "&&", "||", "<|>", "<+>", "<>", "\\"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

-- すべての構文の入口
exprDispatch :: Parser Expr
exprDispatch = do
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< expr dispatch: " ++ show t ++ " ct=" ++ show ct)
  case t of
    TokKeyword "do" -> doExprCore expr -- NoLoop
    TokKeyword "case" -> caseExprCore expr -- NoLoop
    -- TokKeyword "let" -> (try letExpr <|> pLetExpr)
    TokKeyword "let" -> letBlock
    TokKeyword "if" -> ifExpr
    TokKeyword "for" -> forExpr
    TokKeyword "return" -> returnExpr
    TokKeyword "sql" -> parseSQL
    TokSymbol "[" -> listExprCore expr <|> exprCore -- NoLoop
    -- TokSymbol ";" -> parensExpr
    -- TokSymbol "(" -> try exprCore <|> parens expr -- <|> exprCore
    TokSymbol "(" -> try parensExpr <|> try (parens exprCore) <|> exprCore
    -- TokSymbol "(" -> try (parens expr) <|> exprCore
    -- TokSymbol "{" -> bracesExpr
    TokSymbol "\\" -> lambdaExpr
    -- TokVNl -> skipVNlExpr
    TokLambdaCase -> lambdaCaseExpr expr -- NoLoop
    -- _ -> try bracesv1Expr <|> exprCore
    _ -> try bracesExpr <|> try exprCore <|> try (listExprCore expr) -- <|> parensExpr

parensExpr :: Parser Expr
parensExpr = do
  ct <- getRemainingCount
  -- skipNL
  myTrace ("<< parensExpr: ct=" ++ show ct)
  symbol "("
  skipNL
  e <- expr
  skipNL
  symbol ")"
  return e

bracesExpr :: Parser Expr
bracesExpr = do
  skipNL
  ct <- getRemainingCount
  -- myTrace ("<< bracesExpr: ct="++ show ct)
  nm <- typeIdent
  myTrace ("<< bracesExpr: nm " ++ show nm ++ " ct=" ++ show ct)
  skipNL
  symbol "{"
  skipNL
  xs <- many $ do
    e <- ident
    symbol "="
    x <- expr
    optional (symbol ",")
    skipNL
    return (e, x)
  symbol "}"
  return (ERecordConstr nm xs)

whereBlock :: Parser (Maybe [Binding])
whereBlock = try whereClause <|> emptyClause

-- この２つの関数が重要
emptyClause :: Parser (Maybe a)
emptyClause = do
  -- ct <- getRemainingCount
  -- myTrace ("<< emptyClause: no where!!! ct=" ++ show ct)
  -- skipNL
  return Nothing

whereClause :: Parser (Maybe [Binding])
whereClause = do
  skipVNL
  keyword "where"
  bracesV $ do
    Just <$> whBindings -- Block

whBindings :: Parser [Binding]
whBindings = do
  b <- binding
  bs <- many binding
  myTrace (">>*whereClause (b:bs) " ++ show (b : bs))
  return (b : bs)

bindingsBlock :: Parser [Binding]
bindingsBlock = do
  t <- lookAhead anyToken
  myTrace ("<< bindingBlock: next token " ++ show t)
  rt <- bindings -- braces bindings <|> bindings
  t <- lookAhead anyToken
  myTrace (">>*bindingBlock next token " ++ show t ++ " rt " ++ show rt)
  return rt
  where
    bindings = do
      f <- binding
      -- bracesV $ do
      xs <- many binding
      return (f : xs)

letBlock :: Parser Expr
letBlock = do
  t <- lookAhead anyToken
  myTrace ("<< letBlock: next token " ++ show t)
  rt <- try letExpr <|> pLetExpr
  myTrace (">>*letBlock: rt " ++ show rt)
  return rt

binding :: Parser Binding
binding = do
  skipNL
  t <- lookAhead anyToken
  myTrace ("<< binding: next token " ++ show t)
  rt <- try valueBinding <|> funBinding
  myTrace (">>*binding: rt " ++ show rt)
  return rt

letExpr :: Parser Expr
letExpr = do
  t <- lookAhead anyToken
  myTrace ("<< letExpr: next token " ++ show t)
  keyword "let"
  binds <- bindingsBlock
  skipNL
  -- binds <- manyTill binding (lookAhead (token TokVLBrace))
  t <- lookAhead anyToken
  myTrace ("<< letExpr: next token " ++ show t ++ " binds " ++ show binds)
  case t of
    TokVLBrace ->
      bracesV $ do
        keyword "in"
        body <- expr -- NoLoop
        myTrace (">>*letExpr: in body " ++ show body)
        -- skipNewlines
        return (ELetBlock binds body)
    (TokKeyword "in") -> do
      keyword "in"
      body <- expr -- NoLoop
      myTrace (">>*letExpr: in body " ++ show body)
      -- optional (token TokVRBrace)
      -- skipNewlines
      return (ELetBlock binds body)
    _ -> do
      return (ELetBlock binds (EVar "__unit__"))

lambdaExpr :: Parser Expr
lambdaExpr = do
  symbol "\\"
  arg <- pattern
  skipNL
  myTrace ("<< lambdaExpr arg " ++ show arg)
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  skipNL
  body <- expr
  return (ELam arg body)

ifExpr :: Parser Expr
ifExpr = do
  myTrace ("<< ifExpr")
  keyword "if"
  cond <- expr -- Core -- NoBraces -- expr -- NoLoop
  skipNL
  -- t <- lookAhead anyToken
  myTrace ("<< ifExpr cond " ++ show cond)
  keyword "then"
  th <- expr -- NoLoop
  skipNL
  myTrace ("<< ifExpr th " ++ show th)
  keyword "else"
  el <- expr -- NoLoop
  myTrace ("<< ifExpr el " ++ show el)
  return (EIf cond th el)

returnExpr :: Parser Expr
returnExpr =
  try returnWithDollar <|> returnSimple

-- return $ expr
returnWithDollar :: Parser Expr
returnWithDollar = do
  myTrace ("<< returnWithDollar")
  keyword "return"
  token (TokOperator "$")
  e <- expr
  myTrace (">>*return $: " ++ show e)
  return (EReturn e)

returnSimple :: Parser Expr
returnSimple = do
  keyword "return"
  e <- atomCore -- or exprNoLoop
  myTrace (">>*return atom: " ++ show e)
  return (EReturn e)

forExpr :: Parser Expr
forExpr = do
  keyword "for"
  qs <- sepBy1 qualifier (symbol ",")
  token TokArrow
  skipNL
  body <- expr -- NoLoop
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  skipNL
  keyword "in"
  skipNL
  src <- expr -- NoLoop
  return (QGenerator pat src)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> exprNoLoop

funBinding :: Parser Binding
funBinding = do
  skipVNL
  name <- ident
  -- 次のトークンが "=" なら funBinding ではない
  next <- lookAhead anyToken
  myTrace ("<< funBinding: name " ++ show name ++ " next " ++ show next)
  case next of
    TokOperator "=" -> empty -- ← guard の代わりに empty を使う
    _ -> do
      args <- many pattern
      myTrace ("<< funBinding: args " ++ show args)
      skipNL
      symbol "="
      skipNL
      body <- expr -- NoLoop
      myTrace (">>*funBinding: body " ++ show body)
      return (PApp (PVar name) args, body)

valueBinding :: Parser Binding
valueBinding = do
  skipVNL
  pat <- pattern
  skipNL
  myTrace ("<< valueBinding: pat " ++ show pat)
  symbol "="
  skipNL
  body <- expr -- NoLoop
  t <- lookAhead anyToken
  myTrace (">>*valueBinding: next token " ++ show t ++ " body " ++ show body)
  return (pat, body)

pLetExpr :: Parser Expr
pLetExpr = do
  t <- lookAhead anyToken
  myTrace ("<< pLetExpr: next token " ++ show t)
  keyword "let"
  -- t <- lookAhead anyToken
  pat <- pattern
  myTrace ("<< pLetExpr: pat " ++ show pat)
  skipNL
  symbol "="
  -- X bracesV $ do
  e1 <- expr
  skipNL
  -- NoLoop -- ここがNoLoopでないと脱出できない
  keyword "in"
  skipNL
  e2 <- expr -- NoLoop
  myTrace (">>*pLetExpr: e1 " ++ show e1 ++ " e2 " ++ show e2)
  return (ELet pat e1 e2)
