{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import DynFlags
import GHC
import GHC.Paths (libdir)
import Outputable
import System.Environment

data Export = Export
  { name :: String,
    typ :: String
  }
  deriving (Show)

instance ToJSON Export where
  toJSON (Export n t) = object ["name" .= n, "type" .= t]

main :: IO ()
main = do
  [modName] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags

    target <- guessTarget modName Nothing
    setTargets [target]
    _ <- load LoadAllTargets

    modSum <- getModSummary $ mkModuleName modName
    p <- parseModule modSum
    t <- typecheckModule p

    let tcg = tm_internals_ t
        exports = map (toExport . unLoc) (tcg_exports tcg)

    liftIO $ BL.putStrLn (encode exports)

toExport :: Name -> Export
toExport n =
  let nStr = showSDocUnsafe (ppr n)
      tStr = showSDocUnsafe (ppr (varType n))
   in Export nStr tStr
