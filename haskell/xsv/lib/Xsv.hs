module Xsv where

import Data.Tuple (swap)

newtype Value = Value String deriving (Show, Eq)

newtype Field = Field String deriving (Show, Eq)

newtype Header = Header [Field] deriving (Show, Eq)

newtype Row = Row [Value] deriving (Show, Eq)

data Xsv = Xsv
  { xsvHeader :: Maybe Header,
    xsvRows :: [Row],
    xsvDelimiter :: Char
  }
  deriving (Show, Eq)

xsv :: Char -> Xsv
xsv = Xsv Nothing []

data Error = FileNotFound | CannotParse
  deriving (Show, Eq)

splitByDelimiter :: Xsv -> String -> [String]
splitByDelimiter x = uncurry (:) . swap . foldr (\c (acc, vs) -> if c == xsvDelimiter x then (vs : acc, "") else (acc, c : vs)) ([], "")

deserialize :: Bool -> String -> Xsv
deserialize withHeader src = undefined

parse :: FilePath -> IO (Either Error Xsv)
parse src = do
  contents <- readFile src
  undefined
