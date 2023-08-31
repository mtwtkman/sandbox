{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where
-- question: https://stackoverflow.com/a/10242673
-- answer: https://gist.github.com/danoneata/f46bfb5dc3ad2f15667c2024ff5178be
import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Data.List (intersperse)
import Prelude hiding (all)

data Triple a = Tr a a a
  deriving (Show)

instance Functor Triple where
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure a = Tr a a a
  Tr f g h <*> Tr a b c = Tr (f a) (g b) (h c)

instance Foldable Triple where
  foldMap f (Tr a b c) = mconcat [f a, f b, f c]

instance Traversable Triple where
  traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c

newtype (:.) f g x = Comp {comp :: f (g x)}

instance (Functor f, Functor g) => Functor (f :. g) where
  fmap f (Comp x) = Comp $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (f :. g) where
  pure = Comp . pure . pure
  Comp f <*> Comp x = Comp $ fmap (<*>) f <*> x

instance (Foldable f, Foldable g) => Foldable (f :. g) where
  foldMap f (Comp x) = foldMap (foldMap f) x

instance (Traversable f, Traversable g) => Traversable (f :. g) where
  traverse f (Comp x) = Comp <$> traverse (traverse f) x

type Zone = Triple :. Triple

type Board = Zone :. Zone

rows :: Board a -> Board a
rows = id

cols :: Board a -> Board a
cols = Comp . sequenceA . comp

boxs :: Board a -> Board a
boxs = Comp . Comp . fmap ((fmap Comp . sequenceA) . fmap comp) . comp . comp

newtype Parse x = Parser {parse :: String -> [(x, String)]} deriving (Semigroup, Monoid)

instance Functor Parse where
  fmap f (Parser x) = Parser $ fmap (first f) . x

instance Applicative Parse where
  pure x = Parser $ \s -> [(x, s)]
  Parser f <*> Parser x = Parser $ \s -> [(s' s'', r') | (s', r) <- f s, (s'', r') <- x r]

instance Alternative Parse where
  empty = mempty
  (<|>) = mappend

ch :: (Char -> Bool) -> Parse Char
ch p = Parser f
  where
    f (x : xs) | p x = [(x, xs)]
    f _ = []

whitespace :: Parse Char
whitespace = ch (== ' ')

digit :: Parse Char
digit = ch $ flip elem ['0' .. '9']

then' :: Parse Char -> Parse String -> Parse String
then' p q = (:) <$> p <*> q

rep :: Parse Char -> Parse String
rep p = then' p (rep p) <|> pure ""

square :: Parse Int
square = digitToInt <$> ((\_ x -> x) <$> rep whitespace <*> digit)

board :: Parse (Board Int)
board = traverse (const square) (pure 0 :: Board Int)

b_ :: String
b_ =
  mconcat $
    intersperse
      " "
      [ "0 0 0 0 0 0 6 8 0",
        "0 0 0 0 7 3 0 0 9",
        "3 0 9 0 0 0 0 4 5",
        "4 9 0 0 0 0 0 0 0",
        "8 0 3 0 5 0 9 0 2",
        "0 0 0 0 0 0 0 3 6",
        "9 6 0 0 0 0 3 0 8",
        "7 0 0 6 8 0 0 0 0",
        "0 2 8 0 0 0 0 0 0"
      ]

newtype K a x = K {unK :: a}
  deriving (Show)

instance (Monoid a) => Functor (K a) where
  fmap _ (K a) = K a

instance (Monoid a) => Applicative (K a) where
  pure _ = K mempty
  K a <*> K b = K $ a <> b

crush :: (Traversable f, Monoid b) => (a -> b) -> f a -> b
crush f t = unK $ traverse (K . f) t

newtype Any = Any {unAny :: Bool} deriving (Show)

instance Semigroup Any where
  (Any x) <> (Any y) = Any $ x || y

instance Monoid Any where
  mempty = Any False

newtype All = All {unAll :: Bool} deriving (Show)

instance Semigroup All where
  (All x) <> (All y) = All $ x && y

instance Monoid All where
  mempty = All False

any :: (Traversable t) => (a -> Bool) -> t a -> Bool
any p = unAny . crush (Any . p)

all :: (Traversable t) => (a -> Bool) -> t a -> Bool
all p = unAll . crush (All . p)

duplicates :: (Traversable f, Eq a) => f a -> [a]
duplicates = fmap fst . filter ((> 1) . snd) . foldr count []

count :: (Eq a) => a -> [(a, Int)] -> [(a, Int)]
count x [] = [(x, 1)]
count x ((x', c) : xs)
  | x == x' = (x', c + 1) : xs
  | otherwise = (x', c) : count x xs

complete :: Board Int -> Bool
complete = all (`elem` [1 .. 9])

ok :: Board Int -> Bool
ok t = all (\f -> null $ duplicates $ f t) [rows, cols, boxs]

main :: IO ()
main = return ()
