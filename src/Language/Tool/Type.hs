{-# LANGUAGE DeriveGeneric #-}

module Language.Tool.Type where

import Data.Aeson -- (FromJSON)
import qualified Data.Map as Map
import GHC.Generics -- (Generic)

data TypeDBJson = TypeDBJson
  { module_ :: Maybe String,
    types :: [String],
    classes :: [String],
    instances :: [String]
  }
  deriving (Show, Generic)

instance FromJSON TypeDBJson where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \s ->
            case s of
              "module_" -> "module"
              other -> other
        }

{-}
data ModuleTypeInfo = ModuleTypeInfo
  { moduleName :: String,
    types :: [String],
    classes :: [String],
    instances :: [String]
  }
  deriving (Show, Generic)

instance FromJSON ModuleTypeInfo

--type TypeDBJson = Map.Map String ModuleTypeInfo
-}
