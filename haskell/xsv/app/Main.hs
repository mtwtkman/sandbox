module Main where

import Xsv (parse, headeredParse)

main :: IO ()
main = do
  empty <- parse "./examples/empty.csv" ','
  print empty

  normal <- parse "./examples/normal.csv" ','
  print normal

  escaped <- parse "./examples/escaped.csv" ','
  print escaped

  hempty <- headeredParse "./examples/empty.csv" ','
  print hempty

  hnormal <- headeredParse "./examples/normal.csv" ','
  print hnormal

  hescaped <- headeredParse "./examples/escaped.csv" ','
  print hescaped
