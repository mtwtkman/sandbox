{-# LANGUAGE BangPatterns #-}

module ModifyMVarStrict where

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, throw)

run :: MVar a -> (a -> IO a) -> IO ()
run m io = do
  a <- takeMVar m
  !b <-
    io a `catch` \e ->
      putMVar m a >> throw (e :: SomeException)
  putMVar m b
