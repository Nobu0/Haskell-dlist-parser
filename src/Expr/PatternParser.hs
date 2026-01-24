{-# LANGUAGE LambdaCase #-}

module Expr.PatternParser
  ( pattern,
    pConstrOrVar,
    pParenOrTuple,
    pList,
    pWildcard,
    pInt,
  )
where

import Control.Applicative
import Data.Char (isUpper)
import Debug.Trace (trace)
import Expr.AST
import Expr.Combinator
import Expr.TokenParser
import Lexer (Token (..))

{-
pattern :: Parser Pattern
pattern = makeCons

pattern :: Parser Pattern
pattern = trace ">> pattern" (pure ()) *> makeCons

makeCons :: Parser Pattern
makeCons = do
  trace ">> makeCons" (pure ())
  hd <- pAtom
  rest <- optional (symbol ":" *> pattern)
  case rest of
    Just tl -> trace "<< makeCons: cons" (pure (PCons hd tl))
    Nothing -> trace "<< makeCons: atom" (pure hd)
-}

pattern :: Parser Pattern
pattern = do
  p <- makeCons
  trace ("<< pattern: " ++ show p) (pure p)

makeCons :: Parser Pattern
makeCons = do
  hd <- pAtom
  tl <- many pAtom
  return (foldl PApp hd tl)

pAtom :: Parser Pattern
pAtom =
  trace ">> pWildcard" pWildcard
    <|> trace ">> pList" pList
    <|> trace ">> pParenOrTuple" pParenOrTuple
    <|> trace ">> pConstrOrVar" pConstrOrVar
    <|> trace ">> pInt" pInt

{-}
pConstrOrVar :: Parser Pattern
pConstrOrVar = do
  name <- ident
  args <- many pAtom
  if isUpper (head name)
    then return (PConstr name args)
    else case args of
      [] -> return (PVar name)
      _ -> error $ "変数 " ++ name ++ " に引数がついています"
pConstrOrVar :: Parser Pattern
pConstrOrVar = do
  t <- satisfy isIdentOnly
  case t of
    TokIdent s -> return (PVar s)
    _ -> empty
-}

pConstrOrVar :: Parser Pattern
pConstrOrVar = tokenIs $ \case
  TokIdent name -> Just (PVar name)
  TokTypeIdent name -> Just (PConstr name [])
  -- TokTypeIdent name -> Just (PConstr name)
  _ -> Nothing

isIdentOnly (TokIdent _) = True
isIdentOnly _ = False

pParenOrTuple :: Parser Pattern
pParenOrTuple = parens $ do
  pats <- pattern `sepBy1` symbol ","
  return $ case pats of
    [single] -> single
    _ -> PTuple pats

pList :: Parser Pattern
pList = PList <$> brackets (pattern `sepBy` symbol ",")

pWildcard :: Parser Pattern
pWildcard = symbol "_" >> return PWildcard

pInt :: Parser Pattern
pInt = PInt <$> int
