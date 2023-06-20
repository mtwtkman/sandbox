module Main where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as Tx
import qualified Data.Text.Lazy as TL

-- Strict Text
stringToText :: String -> Tx.Text
stringToText = Tx.pack

textToString :: Tx.Text -> String
textToString = Tx.unpack

textToByteString :: Tx.Text -> BC.ByteString
textToByteString = BC.pack . Tx.unpack

-- Lazy Text
stringToLazyText :: String -> TL.Text
stringToLazyText = TL.pack

lazyTextToString :: TL.Text -> String
lazyTextToString = TL.unpack

-- Strict ByteString
stringToByteString :: String -> BC.ByteString
stringToByteString = BC.pack

byteStringToString :: BC.ByteString -> String
byteStringToString = BC.unpack

byteStringToText :: BC.ByteString -> Tx.Text
byteStringToText = Tx.pack . BC.unpack

-- Lazy ByteString
stringToLazyByteString :: String -> BLC.ByteString
stringToLazyByteString = BLC.pack

lazyByteStringToString :: BLC.ByteString -> String
lazyByteStringToString = BLC.unpack

main :: IO ()
main = do
  let ascii = "abc"
      unicode = "あいうえお"
  putStrLn $ Tx.unpack (stringToText ascii)
  putStrLn $ Tx.unpack (stringToText unicode)
  putStrLn $ TL.unpack (stringToLazyText ascii)
  putStrLn $ TL.unpack (stringToLazyText unicode)
  putStrLn $ BC.unpack (stringToByteString ascii)
  putStrLn $ BLC.unpack (stringToLazyByteString ascii)
