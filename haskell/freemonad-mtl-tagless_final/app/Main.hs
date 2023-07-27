module Main where

import FreeMonad

main :: IO ()
main = do
  putStr (showProgram (program :: Free (Toy Char) ()))
  pretty (output 'A')
