module Main where

import Data.List (intercalate)
import Test.Tasty
import Test.Tasty.HUnit
import Xsv (splitByDelimiter, xsv)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Xsv Tests" [splitByDelimiterTest]

splitByDelimiterTest :: TestTree
splitByDelimiterTest =
  testGroup
    "splitByDelimiter"
    [ suite "sequencial value" ',' ["a", "b", "c"],
      suite "sequancial value (by another delimiter)" '-' ["a", "b", "c"],
      suite "though empty" ',' [""],
      suite "though blank head value" ',' ["", "a"],
      suite "though blank tail value" ',' ["a", ""],
      suite "though blank middle value" ',' ["a", "", "b"]
    ]
  where
    suite :: String -> Char -> [String] -> TestTree
    suite msg d e =
      testCase ("splits correctly " <> msg) $
        assertBool
          "equals inversion"
          (splitByDelimiter (xsv d) (intercalate [d] e) == e)
