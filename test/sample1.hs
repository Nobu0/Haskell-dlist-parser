peekToken :: Parser Token
peekToken = Parser $ \tokens ->
  case tokens of
    [] -> Nothing
    (t : _) -> Just (t, tokens)

{-}
groupDecls :: [Decl] -> M.Map Name [Decl]
groupDecls decls =
  M.fromListWith (++) [(name, [d]) | d@(DeclFun name _ _ _ _) <- decls]

isKeyword :: String -> Bool
isKeyword s =
  s `elem` [ "case",
             "of",
             "let",
             "in",
             "if",
             "then",
             "else",
             "do",
             "return"
           ]

operatorVar :: Parser Expr
operatorVar = do
  op <- satisfyToken isOp
  return (EVar op)
  where
    isOp (TokOperator s)
      | s `elem` [":"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

freshTypeVar :: Either InferError Type
freshTypeVar =
  Right
    ( TVar
        ( "t"
            ++ show
              ( unsafePerformIO
                  ( do
                      n <- readIORef counter
                      writeIORef counter (n + 1)
                      return n
                  )
              )
        )
    )

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)



isKeyword :: String -> Bool
isKeyword s =
  s
    `elem` [ "case",
             "of",
             "let",
             "in",
             "if",
             "then",
             "else",
             "do",
             "return"
           ]

isIdentOnly :: Token -> Bool
isIdentOnly (TokIdent _) = True
isIdentOnly _ = False

operatorVar :: Parser Expr
operatorVar = do
  op <- satisfyToken isOp
  return (EVar op)
  where
    isOp (TokOperator s)
      | s `elem` [":"] = Just s
      | otherwise = Nothing
    isOp _ = Nothing

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Just (a, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser $ \input ->
    case runParser pf input of
      Just (f, rest1) ->
        case runParser pa rest1 of
          Just (a, rest2) -> Just (f a, rest2)
          Nothing -> Nothing
      Nothing -> Nothing

instance Pretty Type where
  pretty ty =
    case ty of
      TVar v -> pretty v
      TCon c -> pretty c
      TUnit -> pretty "Unit"
      TArrow a b ->
        parens (pretty a <+> pretty "->" <+> pretty b)
      TList t ->
        brackets (pretty t)
      TApp a b ->
        pretty a <+> pretty b
      TForall vs t ->
        pretty "forall" <+> hsep (map pretty vs) <> pretty "." <+> pretty t
      TTuple ts ->
        tupled (map pretty ts)
      TBinOp op t1 t2 ->
        parens (pretty t1 <+> pretty (prettyPrintBinOp op) <+> pretty t2)
      TConstraint cs t ->
        parens (hsep (punctuate (pretty ",") (map (pretty . show) cs))) <+> pretty "=>" <+> pretty t


inferSQL :: String -> [Expr] -> SQLInfo
inferSQL sqla args =
  SQLInfo
    { 
      sqlText = sqla,
      sqlVars = map getVarName args,
      sqlExprs = args
    }


counter :: IORef Int
counter = unsafePerformIO (newIORef 0)

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


constraintP :: Parser Pattern
constraintP = tokenIs $ \case
  TokTypeIdent name -> Just (PConstr name [])
  _ -> Nothing


exprCore :: Parser Expr
exprCore = do
  rt <-
    try lambdaExpr
      <|> try binOpExprCore
      <|> parseSQL
  return rt

parseBinOp :: String -> Maybe BinOp
parseBinOp s = case s of
  "+" -> Just BinOpAdd
  "-" -> Just BinOpSub
  "*" -> Just BinOpMul
  "/" -> Just BinOpDiv
  "==" -> Just BinOpEq
  "/=" -> Just BinOpNeq
  "!=" -> Just BinOpNeq
  "<" -> Just BinOpLt
  ">" -> Just BinOpGt
  "<=" -> Just BinOpLe
  ">=" -> Just BinOpGe
  "&&" -> Just BinOpAnd
  "||" -> Just BinOpOr
  "++" -> Just BinOpConcat
  ":" -> Just BinOpCons
  "." -> Just BinOpCompose
  "*>" -> Just BinOpThen
  "<*" -> Just BinOpThen
  "<?>" -> Just BinOpThen
  "<$" -> Just BinOpThen
  "<*>" -> Just BinOpThen
  ">>" -> Just BinOpThen
  ">>=" -> Just BinOpBind
  "<|>" -> Just BinOpAlt
  "<$>" -> Just BinOpFmap
  "$" -> Just BinOpApp
  _ -> Nothing

declDispatch :: Parser Decl
declDispatch = do
  ct <- getRemainingCount
  t <- lookAhead anyToken
  myTrace ("<< decl dispatch: " ++ show t ++ " ct=" ++ show ct)
  case t of
    TokIdent _ -> try funDecl <|> try typeSigDecl <|> valueDecl
    TokKeyword "data" -> dataDecl
    TokKeyword "newtype" -> newtypeDecl
    TokKeyword "import" -> importDecl
    TokKeyword "instance" -> instanceDecl
    TokKeyword "module" -> moduleDecl
    TokKeyword "class" -> classDecl
    TokKeyword "type" -> typeDecl
    TokLambdaCase -> empty
    TokSymbol "(" -> try typeSigDecl -- <|> parens decl
    _ -> do
      myTrace ("<< unknown token in decl: " ++ show t)
      empty



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
