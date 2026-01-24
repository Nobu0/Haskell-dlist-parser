{-# LANGUAGE CPP #-}

module MyTrace (myTrace, myTraceIO, myTraceShowId) where

import Debug.Trace (trace, traceIO, traceShowId)

#ifdef ENABLE_TRACE
myTrace :: String -> a -> a
myTrace = trace

myTraceIO :: String -> IO ()
myTraceIO = traceIO

myTraceShowId :: Show a => a -> a
myTraceShowId = traceShowId
#else
myTrace :: String -> a -> a
myTrace _ x = x

myTraceIO :: String -> IO ()
myTraceIO _ = pure ()

myTraceShowId :: Show a => a -> a
myTraceShowId = id
#endif
