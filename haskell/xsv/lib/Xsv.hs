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

splitByDelimiter :: Char -> String -> [String]
splitByDelimiter d = uncurry (:) . swap . foldr (\c (acc, vs) -> if c == d then (vs : acc, "") else (acc, c : vs)) ([], "")

makeLines :: String -> Char -> [[String]]
makeLines s c = map (splitByDelimiter c) $ lines s

makeRows :: [[String]] -> [Row]
makeRows = map (Row . map Value)

makeHeader :: [String] -> Header
makeHeader = Header . map Field

parseHeader :: Xsv -> String -> Xsv
parseHeader x s =
  let d = xsvDelimiter x
   in Xsv
        (Just $ makeHeader (splitByDelimiter d s))
        (xsvRows x)
        d

parseRow :: Xsv -> String -> Xsv
parseRow x s =
  let d = xsvDelimiter x
   in Xsv
        (xsvHeader x)
        (makeRows $ makeLines s d)
        d

addRow :: Xsv -> Row -> Xsv
addRow x row = Xsv (xsvHeader x) (xsvRows x <> [row]) (xsvDelimiter x)

setHeader :: Xsv -> Header -> Xsv
setHeader x h = Xsv (Just h) (xsvRows x) (xsvDelimiter x)

parse :: FilePath -> IO (Either Error Xsv)
parse src = do
  contents <- readFile src
  undefined
