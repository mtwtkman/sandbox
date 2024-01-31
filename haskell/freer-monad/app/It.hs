module It where

import Control.Monad ((>=>))

data It i a
  = Pure a
  | Get (i -> It i a)

ask :: It i i
ask = Get Pure

instance Functor (It i) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Get k) = Get (fmap (fmap f) k)

instance Applicative (It i) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Get k = Get (fmap (fmap f) k)
  Get f <*> Pure a = Get (fmap (\f' -> f' a) . f)
  Get f <*> Get g = Get (\i -> f i <*> g i)

instance Monad (It i) where
  Pure x >>= k = k x
  Get k' >>= k = Get (k' >=> k)

addGet :: Int -> It Int Int
addGet x = ask >>= \i -> return (i + x)

addN :: Int -> It Int Int
addN n = foldl (>=>) return (replicate n addGet) 0

runReader :: i -> It i a -> a
runReader _ (Pure v) = v
runReader x (Get k) = runReader x (k x)

feedAll :: [i] -> It i a -> a
feedAll _ (Pure v) = v
feedAll [] _ = error "end of stream"
feedAll (h : t) (Get k) = feedAll t (k h)
