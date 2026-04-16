module Language.TypeSystem.ClassEnv
  ( ClassEnv (..),
    initialClassEnv,
    isValidInstance,
    addInstance,
  )
where

import qualified Data.Map as Map
import Language.TypeSystem.Syntax -- TCon などを使うなら

type ClassName = String

type TypeName = String

data ClassEnv = ClassEnv
  { classes :: Map.Map ClassName [TypeName]
  }
  deriving (Show)

-- 初期のクラス環境
initialClassEnv :: ClassEnv
initialClassEnv =
  ClassEnv
    { classes =
        Map.fromList
          [ ("Num", ["Int", "Float", "Double"]),
            ("Eq", ["Int", "Bool", "Char"]),
            ("Ord", ["Int", "Char"])
          ]
    }

-- 制約が満たされているかを確認
isValidInstance :: ClassEnv -> ClassName -> Type -> Bool
isValidInstance env cls (TCon name) =
  case Map.lookup cls (classes env) of
    Just tys -> name `elem` tys
    Nothing -> False
isValidInstance _ _ _ = False -- 型変数などは保留

-- インスタンスを追加（将来の拡張用）
addInstance :: ClassName -> TypeName -> ClassEnv -> ClassEnv
addInstance cls ty env =
  let updated = Map.insertWith (++) cls [ty] (classes env)
   in env {classes = updated}