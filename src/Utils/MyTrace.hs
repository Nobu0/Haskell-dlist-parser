{- NOINLINE traceEnabled -}

module Utils.MyTrace (myTrace, myTraceIO, myTraceShowId, traceEnabled, setTrace) where

import Control.Monad (when)
import Data.IORef
import Debug.Trace (trace, traceIO, traceShowId)
import Parser.Core.Combinator (Parser (..))
import System.IO.Unsafe (unsafePerformIO)

traceEnabled :: IORef Bool
traceEnabled = unsafePerformIO (newIORef True)

setTrace :: Bool -> IO ()
setTrace b = writeIORef traceEnabled b

myTrace :: String -> Parser ()
myTrace msg = Parser $ \input -> unsafePerformIO $ do
  enabled <- readIORef traceEnabled
  when enabled (putStrLn msg)
  return (Just ((), input))

myTraceIO :: String -> IO ()
myTraceIO = traceIO

myTraceShowId :: (Show a) => a -> a
myTraceShowId = traceShowId
