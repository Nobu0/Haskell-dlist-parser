{-# LANGUAGE LambdaCase #-}

module Parser.Expr.ExprExtensions
  ( expr,
    exprTop,
    exprCore,
    letExpr,
    ifExpr,
    returnExpr,
    forExpr,
    postfix,
    skipNewlines,
  )
where

-- import Expr.CaseParserCore (caseExpr)

-- import Expr.DoParserCore (doExpr)

-- import Expr.ListParserCore (listExpr)

import AST.Expr
-- import AST.Decl
import AST.Pattern
import Control.Applicative (empty, many, optional, some, (<|>), Alternative (empty))
-- (keyword) -- , whereClause)
import Data.Functor (void)
-- import Decl.DeclParserCore (isEOF)
import Lexer.Token (Token (..))
import Parser.Core.Combinator
import Parser.Core.TokenParser
import Parser.Expr.CaseParserCore (caseExprCore, lambdaCaseExpr)
import Parser.Expr.DoParserCore (doExprCore)
import Parser.Expr.ExprCore -- (atomCore, exprCore, exprCoreNoBraces)
import Parser.Expr.ListParserCore (listExprCore)
import Parser.Expr.PatternParser (pPattern, pattern)
import Parser.SQL.SQLParser
import Utils.MyTrace

-- ============================================
--  exprTop / exprSeq
-- ============================================

-- exprTop :: Parser Expr
-- exprTop = try exprSeq <|> expr
exprTop :: Parser Expr
exprTop = do
  es <- sepEndBy1 expr exprSep
  return $ if length es == 1 then head es else ESeq es

exprSep :: Parser ()
exprSep = skipMany (symbol ";" <|> newline)

expr :: Parser Expr
expr = do
  e <- infixExpr
  myTrace ("<< expr: e " ++ show e)
  return e

infixExprxx :: Parser Expr
infixExprxx = chainr1 (try exprTerm) infixOp

infixExpr :: Parser Expr
infixExpr = do
  e <- exprTerm
  myTrace ("<< infixExpr: e " ++ show e) -- ここで先読みすると全部失敗するようになる。
  rest <-
    many
      ( do
          op <- infixOp
          e2 <- exprTerm
          return (op, e2)
      )
  return (foldr (\(op, e2) acc -> op acc e2) e rest)

{-}
exprTerm :: Parser Expr
exprTerm = do
  e <- exprNoLoop
  -- ep <- bracesvExpr1 e
  postfix e
-}

exprTerm :: Parser Expr
exprTerm = do
  bracesVO $ do
    f <- exprNoLoop
    bracesVO $ do
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

-- t <- lookAhead anyToken
-- myTrace ("<< postfix: next token " ++ show t)
postfix :: Expr -> Parser Expr
postfix e = do
  mop <- optional operatorA
  myTrace ("<< postfix: operator = " ++ show mop)
  case mop of
    Just "$" -> do
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

-- layoutExpr :: Parser Expr
-- layoutExpr = doBlockExpr <|> expr

layoutExpr :: Parser Expr
layoutExpr = do
  t <- lookAhead anyToken
  case t of
    TokVLBrace -> do
      bracesVO expr
    TokSymbol "{" -> do
      braces expr
    _ -> expr

-- postfixで参照
operatorA :: Parser String
operatorA = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` ["<$>", "..", ":", "$", "<|>"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

-- infixで参照
operatorB :: Parser String
operatorB = satisfyToken isOp
  where
    isOp (TokOperator s)
      | s `elem` [".", ">>", "++", "<?>", ">>=", "*>", "<$", "<*>", "<*", "&&", "||"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

{-}
bracesvExpr :: Parser Expr
bracesvExpr = do
  try bracesv2Expr <|> bracesv1Expr

bracesv2Expr :: Parser Expr
bracesv2Expr = do
  token TokVLBrace
  base <- expr
  token TokVLBrace
  e <- postfix base -- opChain operatorBinOp EBinOp base
  token TokVRBrace
  token TokVRBrace
  skipSeparators
  return e

bracesv1Expr :: Parser Expr
bracesv1Expr = do
  -- bracesV $ do
  base <- exprCore
  token TokVLBrace
  e <- postfix base
  token TokVRBrace
  skipSeparators
  return e
-}
-- すべての構文の入口
exprDispatch :: Parser Expr
exprDispatch = do
  t <- lookAhead anyToken
  myTrace ("<< expr dispatch: " ++ show t)
  case t of
    TokKeyword "do" -> doExprCore expr -- NoLoop
    TokKeyword "case" -> caseExprCore expr -- NoLoop
    -- TokKeyword "let" -> (try letExpr <|> pLetExpr)
    TokKeyword "let" -> letBlock
    TokKeyword "if" -> ifExpr
    TokKeyword "for" -> forExpr
    TokKeyword "return" -> returnExpr
    TokKeyword "sql" -> parseSQL
    TokSymbol "[" -> listExprCore expr -- NoLoop
    -- TokSymbol "(" -> parens expr
    -- TokSymbol "(" -> try exprCore <|> parens expr -- <|> exprCore
    TokSymbol "(" -> try (parens expr) <|> try (parens exprCore) <|> exprCore
    -- TokVRBrace -> skipVNlExpr -- bracesv expr
    -- TokSymbol "{" -> braces expr
    TokSymbol "\\" -> lambdaExpr
    -- TokVNl -> skipVNlExpr
    TokLambdaCase -> lambdaCaseExpr expr -- NoLoop
    -- _ -> try bracesv1Expr <|> exprCore
    _ -> exprCore

skipVNlExpr :: Parser Expr
skipVNlExpr = do
  token TokVRBrace -- skipVNl
  empty

whereBlock = try whereClause <|> emptyClause

emptyClause = do
  optional (symbol ";")
  return Nothing

whereClause :: Parser (Maybe [Binding])
whereClause = do
  -- skipSeparators
  optional (symbol ";")
  bracesV $ do
    keyword "where"
    bracesV $ do
      Just <$> bindings -- Block
  where
    bindings :: Parser [Binding]
    bindings = do
      b <- binding
      -- bs <- many (skipSeparators >> binding)
      -- X bracesV $ do
      bs <- many binding
      -- bs <- sepEndBy binding (symbol ";") --exprSep
      myTrace (">>*whereClause (b:bs) " ++ show (b : bs))
      return (b : bs)

{-}
whereClause :: Parser (Maybe [Binding])
whereClause = do
  -- skipSeparators
  optional (symbol ";")
  bracesV $ do
    mWhere <- optional (try (keyword "where"))
    case mWhere of
      Just _ -> do
        bracesV $ do
          Just <$> bindings -- Block
      Nothing -> return Nothing
  where
    bindings :: Parser [Binding]
    bindings = do
      b <- binding
      -- bs <- many (skipSeparators >> binding)
      -- X bracesV $ do
      bs <- many binding
      -- bs <- sepEndBy binding (symbol ";") --exprSep
      myTrace (">>*whereClause (b:bs) " ++ show (b : bs))
      return (b : bs)
-}

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
      bracesV $ do
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
  -- skipNewlines
  return rt

binding :: Parser Binding
binding = do
  -- skipSeparators
  t <- lookAhead anyToken
  myTrace ("<< binding: next token " ++ show t)
  rt <- try valueBinding <|> funBinding
  myTrace (">>*binding: rt " ++ show rt)
  -- skipNewlines
  return rt

letExpr :: Parser Expr
letExpr = try $ do
  t <- lookAhead anyToken
  myTrace ("<< letExpr: next token " ++ show t)
  keyword "let"
  binds <- bindingsBlock
  -- skipNewlines
  -- binds <- manyTill binding (lookAhead (token TokVLBrace))
  t <- lookAhead anyToken
  myTrace ("<< letExpr: next token " ++ show t ++ " binds " ++ show binds)
  case t of
    TokVRBrace -> do
      token TokVLBrace
      keyword "in"
      body <- expr -- NoLoop
      token TokVRBrace
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
  myTrace ("<< lambdaExpr arg " ++ show arg)
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  -- X bracesV $ do
  body <- expr
  return (ELam arg body)

ifExpr :: Parser Expr
ifExpr = do
  myTrace ("<< ifExpr")
  keyword "if"
  cond <- exprCore -- NoBraces -- expr -- NoLoop
  t <- lookAhead anyToken
  myTrace ("<< ifExpr cond " ++ show cond ++ " t " ++ show t)
  bracesV $ do
    keyword "then"
    th <- expr -- NoLoop
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

-- return expr (just one atom)
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
  -- X bracesV $ do
  body <- expr -- NoLoop
  return (EListComp body qs)

qualifier :: Parser Qualifier
qualifier =
  try genQualifier
    <|> guardQualifier

genQualifier :: Parser Qualifier
genQualifier = do
  pat <- pattern
  -- bracesV $ do
  keyword "in"
  src <- expr -- NoLoop
  return (QGenerator pat src)

guardQualifier :: Parser Qualifier
guardQualifier = QGuard <$> exprNoLoop

funBinding :: Parser Binding
funBinding = do
  name <- ident
  -- 次のトークンが "=" なら funBinding ではない
  next <- lookAhead anyToken
  myTrace ("<< funBinding: name " ++ show name ++ " next " ++ show next)
  case next of
    TokOperator "=" -> empty -- ← guard の代わりに empty を使う
    _ -> do
      args <- many pattern
      myTrace ("<< funBinding: args " ++ show args)
      symbol "="
      -- X bracesV $ do
      body <- expr -- NoLoop
      myTrace (">>*funBinding: body " ++ show body)
      return (PApp (PVar name) args, body)

valueBinding :: Parser Binding
valueBinding = do
  pat <- pattern
  myTrace ("<< valueBinding: pat " ++ show pat)
  symbol "="
  -- bracesV $ do
  -- これがあると他の括弧と相互に関係する
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
  symbol "="
  -- X bracesV $ do
  e1 <- expr -- NoLoop -- ここがNoLoopでないと脱出できない
  bracesV $ do
    keyword "in"
    e2 <- expr -- NoLoop
    myTrace (">>*pLetExpr: e1 " ++ show e1 ++ " e2 " ++ show e2)
    return (ELet pat e1 e2)

{-}
bindingsBlockXX :: Parser [Binding]
bindingsBlockXX = do
  braces (sepBy binding (symbol ";" <|> newline))
    <|> sepBy binding (symbol ";" <|> newline)
opChain :: Parser BinOp -> (BinOp -> Expr -> Expr -> Expr) -> Expr -> Parser Expr
opChain opParser makeExpr lhs = do
  hasOp <- optional opParser
  case hasOp of
    Just op -> do
      rhs <- exprCore
      skipSeparators
      let combined = makeExpr op lhs rhs
      myTrace ("<<--- opChain " ++ show combined)
      opChain opParser makeExpr combined
    Nothing -> return lhs

operatorBinOp :: Parser BinOp
operatorBinOp = satisfyToken matchOp
  where
    matchOp (TokOperator s) = lookup s operatorTable
    matchOp _ = Nothing

operatorTable :: [(String, BinOp)]
operatorTable =
  [ ("<|>", BinOpAlt),
    ("<$>", BinOpMap),
    ("&&", BinOpAnd),
    (">>=", BinOpBind)
  ]
-}
