typeAtom :: Parser Type
typeAtom =
  (parens parseTypeCore) -- 括弧付き型
    <|> (TCon <$> typeIdent)
    <|> tUnitType -- (symbol "()" *> pure TUnit) -- 単位型
    <|> (TVar <$> ident)
    <|> brackets (TList <$> parseTypeCore)
    <|> parensTuple

constrainedType :: Parser Type
constrainedType = do
  symbol "("
  cs <- sepBy1 constraint (symbol ",")
  symbol ")"
  keyword "=>"
  ty <- arrowType
  return (TConstraint cs ty)

tUnitType :: Parser Type
tUnitType = do
  symbol "("
  symbol ")"
  return (TUnit)

parseForall :: Parser Type
parseForall = do
  token TokForall
  vars <- some ident
  token (TokOperator ".")
  body <- parseTypeCore
  return $ TForall vars body

constraint :: Parser Constraint
constraint = do
  cls <- ident
  ty <- typeApp
  return (Constraint cls [ty])

constraintP :: Parser Constraint
constraintP = do
  className <- typeIdent
  args <- some typeAtom
  return $ Constraint className args

constraintList :: Parser [Constraint]
constraintList = do
  try (parens (constraintP `sepBy1` symbol ",")) <|> fmap (: []) constraintP

typeP :: Parser Type
typeP = try forallType <|> typeAtom

forallType :: Parser Type
forallType = do
  token TokForall
  vars <- some ident
  token (TokOperator ".")
  t <- constrainedType
  return (TForall vars t)

----------------------------------------------------------
-- newtypeのために追加
----------------------------------------------------------

constr :: Parser Constraint
constr = try constraintRecord <|> constraintP

constraintRecord :: Parser Constraint
constraintRecord = do
  cname <- typeIdent
  fields <- braces (field `sepBy1` symbol ",")
  return (ConstraintRecord cname fields)

field :: Parser Field
field = do
  name <- ident
  symbol "::"
  ty <- parseTypeCore
  return (Field name ty)

{-}
constraintType :: Parser Type
constraintType = do
  cs <- parens (typeConstraint `sepBy1` symbol ",")
  symbol "=>"
  t <- parseTypeCore
  return (TConstraint cs t)
typeConstraint :: Parser Constraint
typeConstraint = try constraintRecord <|> constraintNormal

constraintNormal :: Parser Constraint
constraintNormal = do
  cname <- typeIdent
  tys <- many typeAtom
  return (Constraint cname tys)

parseTypeCore :: Parser Type
parseTypeCore = makeExprParser typeExpr typeOpTable
-}
