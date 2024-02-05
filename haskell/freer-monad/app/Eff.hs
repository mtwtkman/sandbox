{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Eff where

import Control.Monad ((>=>))
import Data.Kind (Type)

data Union (r :: [Type -> Type]) v where
  UNow :: t v -> Union (t ': r) v
  UNext :: Union (t ': r) v -> Union (any ': t ': r) v

class Member t r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

send :: (Member t r) => t v -> Eff r v
send t = Impure (inj t) (tsingleton Pure)

data FTCQueue m a b where
  Leaf :: (a -> m b) -> FTCQueue m a b
  Node :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b

tsingleton :: (a -> m b) -> FTCQueue m a b
tsingleton = Leaf

(|>) :: FTCQueue m a x -> (x -> m b) -> FTCQueue m a b
t |> r = Node t (Leaf r)

(><) :: FTCQueue m a x -> FTCQueue m x b -> FTCQueue m a b
(><) = Node

type Arr r a b = a -> Eff r b

type Arrs r a b = FTCQueue (Eff r) a b

data Eff r a where
  Pure :: a -> Eff r a
  Impure :: Union r x -> Arrs r x a -> Eff r a

data ViewL m a b where
  TOne :: (a -> m b) -> ViewL m a b
  (:|) :: (a -> m x) -> (FTCQueue m x b) -> ViewL m a b

tviewl :: FTCQueue m a b -> ViewL m a b
tviewl (Leaf r) = TOne r
tviewl (Node t1 t2) = go t1 t2
 where
  go :: FTCQueue m a x -> FTCQueue m x b -> ViewL m a b
  go (Leaf r) tr = r :| tr
  go (Node tl1 tl2) tr = go tl1 (Node tl2 tr)

qApp :: Arrs r b w -> b -> Eff r w
qApp q x = case tviewl q of
  TOne k -> k x
  k :| t -> bind' (k x) t
 where
  bind' :: Eff r a -> Arrs r a b -> Eff r b
  bind' (Pure y) k = qApp k y
  bind' (Impure u q') k = Impure u (q' >< k)

instance Functor (Eff r) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure u q) = Impure u (q |> (Pure . f))

instance Applicative (Eff r) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Impure u q = Impure u (q |> (Pure . f))
  Impure u q <*> Pure a = Impure u (q |> (Pure . ($ a)))
  Impure u q <*> m = Impure u (q |> (`fmap` m))

instance Monad (Eff r) where
  Pure x >>= k = k x
  Impure u q >>= k = Impure u (q |> k)

data Reader i x where
  Get :: Reader i i

ask :: (Member (Reader i) r) => Eff r i
ask = send Get

addGet :: (Member (Reader Int) r) => Int -> Eff r Int
addGet x = ask >>= \i -> return (i + x)

addN :: (Member (Reader Int) r) => Int -> Eff r Int
addN n = foldl (>=>) return (replicate n addGet) 0

decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (UNow x) = Right x
decomp (UNext v) = Left v

runReader :: i -> Eff (Reader i ': r) a -> Eff r a
runReader i = handleRelay return (\Get k -> k i)

qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp g h = h . qApp g

handleRelay :: (a -> Eff r w) -> (forall v. t v -> Arr r v w -> Eff r w) -> Eff (t ': r) a -> Eff r w
handleRelay ret _ (Pure x) = ret x
handleRelay ret h (Impure u q) = case decomp u of
  Right x -> h x k
  Left u' -> Impure u' (tsingleton k)
 where
  k = qComp q (handleRelay ret h)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "This should never happen"

data Writer o x where
  Put :: o -> Writer o ()

tell :: (Member (Writer o) r) => o -> Eff r ()
tell = send . Put

rdwr :: (Member (Reader Int) r, Member (Writer String) r) => Eff r Int
rdwr = do
  tell "begin"
  r <- addN 10
  tell "end"
  return r

runWriter :: Eff (Writer o ': r) a -> Eff r (a, [o])
runWriter =
  handleRelay
    (\x -> return (x, []))
    (\(Put o) k -> k () >>= \(x, l) -> return (x, o : l))

composedRW :: Eff '[Writer o, Reader Integer] a -> (a, [o])
composedRW = run . runReader 10 . runWriter

runStateR :: Eff (Writer s ': Reader s ': r) w -> s -> Eff r (w, s)
runStateR m s = loop s m
 where
  loop :: s -> Eff (Writer s ': Reader s ': r) w -> Eff r (w, s)
  loop s' (Pure x) = return (x, s')
  loop s' (Impure u q) = case decomp u of
    Right (Put o) -> k o ()
    Left u' -> case decomp u' of
      Right Get -> k s' s'
      Left u'' -> Impure u'' (tsingleton (k s'))
   where
    k s'' = qComp q (loop s'')
