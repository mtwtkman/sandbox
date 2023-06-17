module Expensive where

import Control.Concurrent
import Control.Monad (void)

notQuiteRight :: IO ()
notQuiteRight = do
  mv <- newEmptyMVar
  void (forkIO $ expensiveComputation_striter mv)
  someOtherActivity
  result <- takeMVar mv
  print result
  print "done"
  where
    expensiveComputation_striter mv = do
      let a = "this is "
          b = "not really "
          c = "all that expensive"
      putMVar mv (a <> b <> c)

    someOtherActivity = print "hi"
