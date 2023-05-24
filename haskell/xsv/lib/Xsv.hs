module Xsv  where

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

newtype SplittedLine = SplittedLine { splittedLine :: [String] } deriving (Eq, Show)
newtype ProcessedContents = ProcessedContents [SplittedLine] deriving (Eq, Show)

xsv :: Char -> Xsv
xsv = Xsv Nothing []

data Error = NoHeader
  deriving (Show, Eq)

splitByDelimiter :: Char -> String -> SplittedLine
splitByDelimiter d = SplittedLine . uncurry (:) . swap . foldr (\c (acc, vs) -> if c == d then (vs : acc, "") else (acc, c : vs)) ([], "")

process :: String -> Char -> ProcessedContents
process s c = ProcessedContents $ map (splitByDelimiter c) (lines s)

makeRows :: ProcessedContents -> [Row]
makeRows (ProcessedContents s) = map (Row . map Value . splittedLine) s

makeHeader :: SplittedLine -> Header
makeHeader = Header . map Field . splittedLine

parseHeader :: Xsv -> SplittedLine -> Xsv
parseHeader x l = Xsv
                    (Just $ makeHeader l)
                    (xsvRows x)
                    (xsvDelimiter x)

parseRows :: Xsv -> ProcessedContents -> Xsv
parseRows x s = Xsv
                (xsvHeader x)
                (makeRows s)
                (xsvDelimiter x)

parse :: FilePath -> Char -> IO (Either Error Xsv)
parse src c = do
  contents <- readFile src
  let parsed = parseRows (xsv c) (process contents c)
  return $ Right parsed

headeredParse :: FilePath -> Char -> IO (Either Error Xsv )
headeredParse src c = do
  contents <- readFile src
  let processed = process contents c
  case processed of
    ProcessedContents [] -> return $ Left NoHeader
    ProcessedContents (header:rows) ->
      let x = parseHeader (xsv c) header
       in return $ Right (parseRows x (ProcessedContents rows))
