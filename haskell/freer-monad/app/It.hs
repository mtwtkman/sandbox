module It where

import Control.Monad ((<=<))

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
  Get f <*> Get g =
    Get
      ( \i ->
          let f' = f i
              g' = g i
           in f' <*> g'
      )

instance Monad (It i) where
  return = pure
  Pure x >>= k = k x
  Get k' >>= k = Get (k <=< k')
