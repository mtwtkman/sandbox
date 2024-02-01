module RWIt where

import Control.Monad ((>=>))
import Data.Functor ((<&>))

data It i o a
  = Pure a
  | Get (i -> It i o a)
  | Put o (() -> It i o a)

instance Functor (It i o) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Get k) = Get (fmap f . k)
  fmap f (Put x k) = Put x (fmap f . k)

instance Applicative (It i o) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Get k = Get (fmap (fmap f) k)
  Pure f <*> Put x k = Put x (fmap (fmap f) k)
  Get f <*> Pure a = Get (fmap (fmap (\f' -> f' a)) f)
  Get f <*> Get g = Get (\i -> f i <*> g i)
  Get f <*> Put _ g = Get (\i -> f i <*> g ())
  Put x f <*> Pure a = Put x (\() -> (\f' -> f' a) <$> f ())
  Put _ f <*> Get g = Get (\i -> f () <*> g i)
  Put _ f <*> Put x g = Put x (\() -> f () <*> g ())

instance Monad (It i o) where
  Pure x >>= k = k x
  Get k' >>= k = Get (k' >=> k)
  Put x k' >>= k = Put x (k' >=> k)

runRdWriter :: (Monoid o) => i -> It i o a -> (a, o)
runRdWriter i = loop mempty
 where
  loop acc (Pure x) = (x, acc)
  loop acc (Get k) = loop acc (k i)
  loop acc (Put o k) = loop (acc `mappend` o) (k ())

type StateIt s = It s s

ask :: It i o i
ask = Get Pure

addGet :: Int -> It Int o Int
addGet x = ask <&> (+ x)
