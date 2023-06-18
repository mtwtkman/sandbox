module Main where

import Data.Bits (shiftR, (.&.))

-- 010 & 101 ->
pow :: Int -> Int -> Int
pow x n = go x n 1
  where
    go _ 0 acc = acc
    go x n acc =
      go (x * x) (n `shiftR` 1) (acc * (if n .&. 1 == 1 then x else 1))

main :: IO ()
main = print $ pow 100 8
