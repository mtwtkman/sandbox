module MVarExample where

import Control.Concurrent
import Control.Monad (void)

run :: IO ()
run = do
  m <- newEmptyMVar
  void
    ( forkIO $ do
        v <- takeMVar m
        putStrLn ("received " <> show v)
    )
  putStrLn "sending"
  putMVar m "wake up!"
