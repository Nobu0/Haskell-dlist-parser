module TypeInference.SQLInfer where

import AST.Decl (Decl (..))
import AST.Expr (CaseAlt (..), Expr (..), Name, SQLInfo (..))
import AST.Pattern (Pattern (..))
import AST.Type (Type (..))
import TypeInference.Error (InferError (..))
import TypeInference.Subst
import TypeInference.TypeEnv

inferSQL :: String -> [Expr] -> SQLInfo
inferSQL sqla args =
  SQLInfo
    { sqlText = sqla,
      sqlVars = map getVarName args,
      sqlExprs = args
    }

getVarName :: Expr -> String
getVarName (EVar name) = name
getVarName _ = error "SQL arguments must be variables"
