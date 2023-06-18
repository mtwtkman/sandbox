module Main where

import Data.Bits (shiftR, (.&.))

-- 010 & 101 ->
pow :: Int -> Int -> Int
pow x n = go x n 1
  where
    go x n acc
      | n == 0 = acc
      | otherwise = go (x * x) (n `shiftR` 1) (acc * (if n .&. 1 == 1 then x else 1))

main :: IO ()
main = do
  print $ pow 2 3
  print $ pow 7 2
  print $ pow 7 0
