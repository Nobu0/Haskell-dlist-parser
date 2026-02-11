module Main where

-- module Test.TypeInferSQL where

import Lexer.Lexer (runLexer)
-- import Parser.Core.Combinator (Parser (..), runParser)
import Parser.Core.Parser (parseExpr, toplevel)
-- import Parser.Expr.ExprParser (exprTop)

import TypeInference.Infer
import TypeInference.Infer (inferExpr)
import TypeInference.Pretty (prettyType)
import TypeInference.Type (Type (..))
import TypeInference.TypeEnv (emptyEnv)

runInfer :: String -> String -> IO ()
runInfer cm src = do
  putStrLn $ "\n-- test " ++ cm ++ " Input: " ++ src
  let toks = runLexer src
  putStrLn $ "Tokens: " ++ show toks

  let parsed = parseExpr toks
  case parsed of
    Nothing -> putStrLn "Parse failed"
    Just (ast, _) -> do
      putStrLn $ "AST: " ++ show ast
      case inferExpr emptyEnv ast of
        Left err -> putStrLn $ "TypeError: " ++ show err
        Right (_, ty) -> putStrLn $ "Type: " ++ prettyType ty
        _ -> putStrLn $ "error"

inferCase ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Expr ->
  [CaseAlt] ->
  Either InferError (Subst, Type)
inferCase inferExprFn env scrut branches = do
  (sScrut, tScrut) <- inferExprFn env scrut
  results <- mapM (inferBranch inferExprFn env tScrut sScrut) branches
  unifyManyExpr results

inferBranch ::
  (TypeEnv -> Expr -> Either InferError (Subst, Type)) ->
  TypeEnv ->
  Type ->
  Subst ->
  CaseAlt ->
  Either InferError (Subst, Type)
inferBranch inferExprFn env tScrut sScrut (CaseAlt pat expr) = do
  (sPat, envPat, tPat) <- inferPattern pat
  sUnify <- case unify (apply sPat tPat) (apply sPat tScrut) of
    Left uerr -> Left (InferUnifyError uerr)
    Right s -> Right s
  let s = sUnify `composeSubst` sPat `composeSubst` sScrut
  inferExprFn (applyEnv s (mergeEnvs env envPat)) expr

unifyManyExpr :: [(Subst, Type)] -> Either InferError (Subst, Type)
unifyManyExpr [] = Left (InferOther "empty case")
unifyManyExpr ((s, t) : xs) = foldM step (s, t) xs
  where
    step (sAcc, tAcc) (sNext, tNext) = do
      sU <- case unify (apply sAcc tAcc) (apply sAcc tNext) of
        Left uerr -> Left (InferUnifyError uerr)
        Right su -> Right su
      let sFinal = sU `composeSubst` sNext `composeSubst` sAcc
      Right (sFinal, apply sFinal tAcc)

{-}
-- TypeInference/Infer/Expr/ExprCase.hs
module TypeInference.Infer.Expr.ExprCase
  ( inferCase,
    inferBranch,
    unifyManyExpr,
  )
where

import AST.Expr
import AST.Pattern
import AST.Type
import Control.Monad (foldM)
import TypeInference.Error
import TypeInference.Infer.Core
-- import TypeInference.Infer.Expr.CoreExpr (inferExpr)
import TypeInference.Infer.Expr.ExprLet (inferBinding, inferBindings)
import TypeInference.Infer.Pattern
import TypeInference.Subst
import TypeInference.TypeEnv
import TypeInference.Unify (unify)

inferCase :: TypeEnv -> Expr -> [CaseAlt] -> Either InferError (Subst, Type)
inferCase inferExpr env scrut branches = do
  (sScrut, tScrut) <- inferExpr env scrut
  results <- mapM (inferBranch env tScrut sScrut) branches
  unifyManyExpr results

inferBranch :: TypeEnv -> Type -> Subst -> CaseAlt -> Either InferError (Subst, Type)
inferBranch env tScrut sScrut (CaseAlt pat expr) = do
  (sPat, envPat, tPat) <- inferPattern pat
  trace ("inferPattern: " ++ show pat ++ " => " ++ show envPat) $ do
    sUnify <- case unify (apply sPat tPat) (apply sPat tScrut) of
      Left uerr -> Left (InferUnifyError uerr)
      Right s -> Right s
    let s = sUnify `composeSubst` sPat `composeSubst` sScrut
    inferExpr (applyEnv s (mergeEnvs env envPat)) expr

unifyManyExpr :: [(Subst, Type)] -> Either InferError (Subst, Type)
unifyManyExpr [] = Left (InferOther "empty case")
unifyManyExpr ((s, t) : xs) = foldM step (s, t) xs
  where
    step (sAcc, tAcc) (sNext, tNext) = do
      sU <- case unify (apply sAcc tAcc) (apply sAcc tNext) of
        Left uerr -> Left (InferUnifyError uerr)
        Right s -> Right s
      let sFinal = sU `composeSubst` sNext `composeSubst` sAcc
      Right (sFinal, apply sFinal tAcc)
-}