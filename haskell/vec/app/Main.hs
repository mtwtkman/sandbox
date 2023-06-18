{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector as U
import System.Environment

main :: IO ()
main = do
  [f] <- getArgs
  s <- L.readFile f
  print . U.sum . parse $ s

parse :: L.ByteString -> U.Vector Int
parse = U.unfoldr step
  where
    step !s = case L.readInt s of
      Nothing -> Nothing
      Just (!k, !t) -> Just (k, L.tail t)
