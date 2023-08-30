{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Bifunctor (first)

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

newtype Parse x = Parser {parse :: String -> [(x, String)]} deriving (Semigroup, Monoid)

instance Functor Parse where
  fmap f (Parser x) = Parser $ fmap (first f) . x

instance Applicative Parse where
  pure x = Parser $ \s -> [(x, s)]
  Parser f <*> Parser x = Parser $ \s -> [(s' s'', r') | (s', r) <- f s, (s'', r') <- x r]

main :: IO ()
main = return ()
