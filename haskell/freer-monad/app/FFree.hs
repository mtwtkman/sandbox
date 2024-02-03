{-# LANGUAGE GADTs #-}

module FFree where

import Control.Monad ((>=>))

data FFree f a where
  Pure :: a -> FFree f a
  Impure :: f x -> (x -> FFree f a) -> FFree f a

data FReaderWriter i o x where
  Get :: FReaderWriter i o i
  Put :: o -> FReaderWriter i o ()

type IT i o a = FFree (FReaderWriter i o) a

instance Functor (FFree f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure fx k) = Impure fx (fmap f . k)

instance Applicative (FFree f) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Impure fx k = Impure fx (fmap f . k)
  Impure fx f <*> ff = Impure fx (\x -> f x <*> ff)

instance Monad (FFree f) where
  Impure fx k' >>= k = Impure fx (k' >=> k)
  Pure a >>= k = k a
