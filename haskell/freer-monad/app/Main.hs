{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import qualified It

evalIt :: IO ()
evalIt = do
  print $ show $ It.runReader 7 (It.addGet 3)
  print $ show $ It.runReader 10 (It.addN 8)
  print $ show $ It.feedAll [10, 2, 3, 7, 4] (It.addN 3)

main :: IO ()
main = do
  evalIt
