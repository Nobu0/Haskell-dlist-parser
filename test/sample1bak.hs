operatorA :: Parser String
operatorA = satisfyToken isOp where
  isOp (TokOperator s)
    | s `elem` ["<$>", "..", ":", "$"] = Just s
    | otherwise = Nothing
  isOp _ = Nothing



operatorVar :: Parser Expr
operatorVar = 
  do
    op <- satisfyToken isOp
    return (EVar op)
  where
    isOp (TokOperator s)
      | s `elem` [":"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

{-}

oPsectionCore :: Parser Expr
oPsectionCore = do
  try (EOpSectionL <$> operator <*> exprCore)
    <|> (EOpSectionR <$> exprCore <*> operator)



badToken :: Parser ()
badToken =
  choice
    [ symbol "}",
      symbol ";",
      symbol "$",
      tokenIs (\t -> case t of TokLambdaCase -> Just (); _ -> Nothing),
      tokenIs (\t -> case t of TokVRBrace -> Just (); _ -> Nothing)
    ]

int :: Parser Int
int = do
  t <- satisfy isNumber
  case t of
    TokNumber n -> pure n
    _ -> empty
  where
    isNumber (TokNumber _) = True
    isNumber _ = False


manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go = end *> pure [] <|> (:) <$> p <*> go


getNameList :: Parser [String]
getNameList = do
  list
  where
    name = do
      t <- lookAhead anyToken
      myTrace ("<< getNameList: next token " ++ show t)
      nm <- try identI <|> parens operatorIAsName <|> operatorEdName
      myTrace ("<< getNameList:2 next token " ++ show t ++ " " ++ show nm)
      optional (symbol ",")
      return nm
    list = do
      f <- name
      xs <- many1 name
      return (f : xs)

operatorTable :: [(String, BinOp)]
operatorTable =
  [ ("<|>", BinOpAlt),
    ("<$>", BinOpMap),
    ("&&", BinOpAnd),
    (">>=", BinOpBind)
  ]


whereClause :: Parser (Maybe [Binding])
whereClause = do
  -- skipSeparators
  optional (symbol ";")
  bracesV $ do
    keyword "where"
    bracesV $ do
      Just <$> whBindings -- Block

whBindings :: Parser [Binding]
whBindings = do
  b <- binding
  -- bs <- many (skipSeparators >> binding)
  -- X bracesV $ do
  bs <- many binding
  -- bs <- sepEndBy binding (symbol ";") --exprSep
  myTrace (">>*whereClause (b:bs) " ++ show (b : bs))
  return (b : bs)

try :: Parser a -> Parser a
try p = Parser $ \tokens -> runParser p tokens

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

satisfyMap :: (Token -> Maybe a) -> Parser a
satisfyMap f = Parser $ \tokens ->
  case tokens of
    (t : ts) -> case f t of
      Just x -> Just (x, ts)
      Nothing -> Nothing
    [] -> Nothing

isSymbolName :: String -> Bool
isSymbolName s =
  case s of
    ('(' : _) | last s == ')' -> True
    _ -> not (null s) && isSymbolStart (head s)

extractSQLVars :: String -> (String, [String])
extractSQLVars = go "" [] ""
  where
    go acc vars current [] =
      (acc, reverse vars)
    go acc vars current ('{' : xs) =
      let (var, rest) = span (/= '}') xs in go (acc ++ "?") (var : vars) "" (drop 1 rest)
    go acc vars current (x : xs) =
      go (acc ++ [x]) vars current xs

typeAtom :: Parser Type
typeAtom =
  (parens parseTypeCore)
    <|> (TCon <$> typeIdent)
    <|> tUnitType
    <|> (TVar <$> ident)
    <|> brackets (TList <$> parseTypeCore)
    <|> parensTuple

compareAST :: [Char] -> [Char] -> IO ()
compareAST actualRaw expectedRaw = do
  let normalize = filter (not . (`elem` [' ', '\n', '\t']))
  let actual = normalize actualRaw
  let expected = normalize expectedRaw
  if actual == expected
    then putStrLn "  O Passed\n"
    else do
      putStrLn "  X Failed!"
      putStrLn $ "     Expected: " ++ expectedRaw
      putStrLn $ "     Got:      " ++ actualRaw ++ "\n"

constraintList :: Parser [Constraint]
constraintList = do
  try (parens (constraintP `sepBy1` symbol ","))
    <|> fmap (: []) constraintP

caseGuard :: Parser Expr -> Parser (Expr, Expr)
caseGuard expr = do
  t <- lookAhead anyToken
  myTrace ("<< caseGuard next token: " ++ show t)
  symbol "|"
  cond <- expr
  tokenIs (\case TokArrow -> Just (); _ -> Nothing)
  -- token TokArrow
  body <- expr
  myTrace (">>*caseGuard body: " ++ show body)
  return (cond, body)

makeCons :: Parser Pattern
makeCons = do
  p <- makeApp
  rest p
  where
    rest p =
      ( do
          symbol ":"
          p2 <- makeCons
          return (PCons p p2)
      )
        <|> return p

atomCorex :: Parser Expr
atomCorex = do
  t <- lookAhead anyToken
  case t of
    TokSymbol "}" -> empty
    TokSymbol ";" -> empty
    TokSymbol "$" -> empty
    -- TokVRBrace -> empty
    TokLambdaCase -> empty
    _ ->
      try (parens parenExprCore)
        <|> atomBaseCore

exportItem :: Parser Export
exportItem =
  if isUpper (head name)
    then ExportType name False
    else ExportVar name

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

(<?>) :: Parser a -> String -> Parser a
p <?> _ = p

typeAtom :: Parser Type
typeAtom =
  (parens parseTypeCore)
    <|> (TCon <$> typeIdent)
    <|> tUnitType
    <|> (TVar <$> ident)
    <|> brackets (TList <$> parseTypeCore)
    <|> parensTuple

doExprCore :: Parser Expr -> Parser Expr
doExprCore expr = do
  keyword "do"
  bracesV $ do
    optional (token TokNewline)
    stmts <- doBlock expr
    myTrace (">>*doExprCore: stmts " ++ show stmts)
    return (EDo stmts)

doBlock :: Parser Expr -> Parser [Stmt]
doBlock expr = do
  t <- optional (lookAhead anyToken)
  case t of
    Just (TokSymbol "}") -> pure []
    _ -> do
      f <- doStmt expr
      m <- many (doStmt expr)
      myTrace (">>*doBlock: (f:m)" ++ show (f : m))
      return (f : m)

caseLambdaBlock :: Parser Expr -> Parser [CaseAlt]
caseLambdaBlock expr = do
  f <- caseAlt expr
  xs <- many (caseAlt expr)
  return (f : xs)

constraintList :: Parser [Constraint]
constraintList = do
  try $
    parens (constraintP `sepBy1` symbol ",")
      <|> fmap (: []) constraintP

doStmt :: Parser Expr -> Parser Stmt
doStmt expr = do
  rt <-
    try (bindStmt expr)
      <|> try (letStmt expr)
      <|> exprStmt expr
  myTrace (">>*doStmt: rt " ++ show rt)
  skipSeparators
  return rt

exportItem :: Parser Export
exportItem = do
  t <- lookAhead anyToken
  myTrace ("<< exportItem: next token=" ++ show t)
  name <-
    try typeIdent
      <|> try ident
      <|> operatorEdName
      <|> operatorIAsName
  myTrace ("<< exportItem: name " ++ show name)
  hasAll <- optional (parens (symbol ".."))
  return $ case hasAll of
    Just _ -> ExportType name True
    Nothing ->
      if isUpper (head name)
        then ExportType name False
        else ExportVar name

importIdent :: Parser ImportItem
importIdent = do
  t <- lookAhead anyToken
  myTrace ("<< importIdent: next token " ++ show t)
  name <-
    try identI
      <|> operatorEdName
      <|> operatorIAsName
  m <- optional tmp
  optional (symbol ",")
  myTrace ("<< importIdent: m " ++ show m)
  return $ case m of
    Just x -> x
    Nothing -> ImportVar name

tmp =
  parensI (ImportTypeAll name <$ symbol "..")
    <|> (ImportTypeSome name <$> getNameList)

skipBlk :: Parser ()
skipBlk = do
  optional (token TokVLBrace)
  optional (token TokVRBrace)
  optional (token TokNewline)
  optional (token (TokSymbol ";"))
  return ()

typeSigDecl :: Parser Decl
typeSigDecl = do
  t <- lookAhead anyToken
  myTrace ("<< typeSigDecl: " ++ show t)
  name <-
    try ident
      <|> do
        op <- parens operatorI
        return $ "(" ++ op ++ ")"
  t <- lookAhead anyToken
  myTrace ("<< typeSigDecl:2 " ++ show t)
  symbol "::"
  ty <- parseType
  myTrace ("<< parsed type signature: " ++ name ++ " :: " ++ show ty)
  let decl = DeclTypeSig name ty
  myTrace ("<< returning DeclTypeSig: " ++ show decl)
  return decl

decl :: Parser Decl
decl = do
  t <- lookAhead anyToken
  myTrace ("<< decl next token: " ++ show t)
  eof <- isEOF
  if eof
    then Parser $ \_ -> Nothing
    else declBody

parseTypeCore :: Parser Type
parseTypeCore = do
  skipMany (symbol ";" <|> newline)
  t1 <- arrowType
  myTrace ("<< parseTypeCore: " ++ show t1)
  rest <- optional $ do
    token (TokKeyword "=>")
    t2 <- parseTypeCore
    return (t1, t2)
  case rest of
    Just (TApp (TCon cls) arg, body) ->
      return $ TConstraint [Constraint cls [arg]] body
    Just (TCon cls, body) ->
      return $ TConstraint [Constraint cls []] body
    Just (TTuple cs, body) ->
      return $ TConstraint (map toConstraint cs) body
    Nothing -> return t1
    _ -> empty

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
  ( do
      x <- p
      xs <- many (sep *> p)
      return (x : xs)
  )
    <|> pure []
