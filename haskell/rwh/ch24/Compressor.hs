module Compressor where

import Codec.Compression.GZip (compress)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, handle)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as L
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)

run :: IO ()
run = do
  maybeLine <- runInputT defaultSettings $ getInputLine "Enter a file to compress> "
  case maybeLine of
    Nothing -> return ()
    Just "" -> return ()
    Just name -> do
      handle (print :: SomeException -> IO ()) $ do
        content <- L.readFile name
        void (forkIO (compressFile name content))
  where
    compressFile path = L.writeFile (path <> ".gz") . compress
