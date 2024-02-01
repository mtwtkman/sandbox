module Free where

data Free f a = Pure a | Impure (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure k) = Impure (fmap (fmap f) k)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Impure k = Impure (fmap (fmap f) k)
  Impure f <*> Pure a = Impure (fmap (fmap (\f' -> f' a)) f)
  Impure f <*> g = Impure (fmap (<*> g) f)

instance (Functor f) => Monad (Free f) where
  Pure a >>= k = k a
  Impure f >>= k = Impure (fmap (>>= k) f)

data ReaderWriter i o x = Get (i -> x) | Put o (() -> x)

instance Functor (ReaderWriter i o) where
  fmap f (Get k) = Get (f . k)
  fmap f (Put o k) = Put o (const $ f (k ()))

type IT i o a = Free (ReaderWriter i o) a

runReaderWriter :: (Monoid o) => i -> ReaderWriter i o x -> (x, o)
runReaderWriter i = loop mempty
 where
  loop acc (Get k) = (k i, acc)
  loop acc (Put o k) = (k (), acc <> o)

runFree :: (Monad m) => Free m a -> m a
runFree (Pure a) = return a
runFree (Impure f) = f >>= runFree
