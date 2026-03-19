{- NOINLINE traceEnabled -}

module Utils.MyTrace
  ( myTrace,
    myTraceIO,
    myTraceIOM,
    myTraceShowId,
    traceEnabled,
    setTrace,
    myTraceE,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Debug.Trace (trace, traceIO, traceShowId)
import Parser.Core.Combinator (Parser (..))
import System.IO.Unsafe (unsafePerformIO)
import TypeInference.Infer.Core

traceEnabled :: IORef Bool
traceEnabled = unsafePerformIO (newIORef True)

setTrace :: Bool -> IO ()
setTrace b = writeIORef traceEnabled b

myTrace :: String -> Parser ()
myTrace msg = Parser $ \input ->
  unsafePerformIO $ do
    enabled <- readIORef traceEnabled
    when enabled (putStrLn msg)
    return (Just ((), input))

myTraceIO :: String -> IO ()
myTraceIO = traceIO

myTraceShowId :: (Show a) => a -> a
myTraceShowId = traceShowId

myTraceE :: String -> InferM ()
myTraceE msg = do
  let enabled = unsafePerformIO (readIORef traceEnabled)
  when enabled $ do
    _ <- lift $ unsafePerformIO (putStrLn msg >> return (Right ()))
    return ()

{-}
myTraceE :: String -> Either e ()
myTraceE msg = unsafePerformIO $ do
  enabled <- readIORef traceEnabled
  when enabled (putStrLn msg)
  return (Right ())

myTraceShowE :: (Show a) => a -> Either e a
myTraceShowE x = unsafePerformIO $ do
  enabled <- readIORef traceEnabled
  when enabled (print x)
  return (Right x)
-}

myTraceIOM :: String -> IO ()
myTraceIOM msg = do
  enabled <- readIORef traceEnabled
  when enabled (putStrLn msg)
