module Chan where

import Control.Concurrent
import Control.Monad (void)

chanExample :: IO ()
chanExample = do
  ch <- newChan
  void
    ( forkIO $ do
        writeChan ch "hello world"
        writeChan ch "now i quit"
    )
  readChan ch >>= print
  readChan ch >>= print
