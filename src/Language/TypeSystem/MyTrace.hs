{- NOINLINE traceEnabled -}

module Language.TypeSystem.MyTrace
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
import Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
-- (InferState)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, StateT)
import Data.IORef
import Debug.Trace (trace, traceIO, traceShowId)
-- import Language.TypeSystem.Error (InferError)
-- import Language.TypeSystem.InferM
import Parser.Core.Combinator (Parser (..))
import System.IO.Unsafe (unsafePerformIO)

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

myTraceE :: (MonadIO m) => String -> m ()
myTraceE msg = liftIO $ do
  enabled <- readIORef traceEnabled
  when enabled (putStrLn msg)

{-}
myTraceE :: String -> ExceptT InferError (State InferState) ()
myTraceE msg = lift $ lift $ do
  enabled <- readIORef traceEnabled
  when enabled (putStrLn msg)
-}

myTraceIOM :: String -> IO ()
myTraceIOM msg = do
  enabled <- readIORef traceEnabled
  when enabled (putStrLn msg)
