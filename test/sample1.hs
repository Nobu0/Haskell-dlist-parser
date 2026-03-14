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

badToken :: Parser ()
badToken =
  choice
    [ symbol "}",
      symbol ";",
      symbol "$",
      tokenIs (\t -> case t of TokLambdaCase -> Just (); _ -> Nothing),
      tokenIs (\t -> case t of TokVRBrace -> Just (); _ -> Nothing)
    ]

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
