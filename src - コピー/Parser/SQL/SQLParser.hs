module Parser.SQL.SQLParser (parseSQL) where

import AST.Expr -- (Expr (..))

-- {var} 抽出用（後で作る）
import Parser.Core.Combinator
import Parser.Core.TokenParser
-- import Parser.Expr.ExprCore
import Utils.SQLUtils

parseSQL :: Parser Expr
parseSQL = do
  _ <- keyword "sql"
  sqlText <- stringLiteralExpr
  let (sqlBody, vars) = extractSQLVars sqlText
  return (ESQL sqlBody (map EVar vars))
