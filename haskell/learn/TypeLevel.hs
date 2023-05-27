{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module TypeLevel where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Prelude hiding (filter, reverse)

-- ref: https://zenn.dev/mod_poppo/books/haskell-type-level-programming
-- data Expr a
--   = Const Int
--   | Add (Expr Int) (Expr Int)
--   | Equal (Expr Int) (Expr Int)
--   | IfThenElse (Expr Bool) (Expr a) (Expr a)
--   deriving (Show)
--
-- mkConst :: Int -> Expr Int
-- mkConst = Const
--
-- mkAdd :: Expr Int -> Expr Int -> Expr Int
-- mkAdd = Add
--
-- mkEqual :: Expr Int -> Expr Int -> Expr Bool
-- mkEqual = Equal

data Expr (a :: Type) where
  Const :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Equal :: Expr Int -> Expr Int -> Expr Bool
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval e = case e of
  Const x -> x
  Add e1 e2 ->
    eval e1 + eval e2
  Equal e1 e2 ->
    eval e1 == eval e2
  IfThenElse cond then_ else_ ->
    if eval cond then eval then_ else eval else_

class SameNat (n :: PeanoNat) (m :: PeanoNat) where
  sameNat :: Proxy n -> Proxy m -> Maybe (n :~: m)

instance SameNat 'Zero 'Zero where
  sameNat _ _ = Just Refl

instance SameNat 'Zero ('Succ n) where
  sameNat _ _ = Nothing

instance SameNat ('Succ n) 'Zero where
  sameNat _ _ = Nothing

instance SameNat n m => SameNat ('Succ n) ('Succ m) where
  sameNat _ _ = case sameNat (Proxy :: Proxy n) (Proxy :: Proxy m) of
    Just Refl ->
      Just Refl
    Nothing ->
      Nothing

maxBoundAsInteger :: forall a. (Integral a, Bounded a) => Proxy a -> Integer
maxBoundAsInteger _proxy = toInteger (maxBound :: a)

data PeanoNat = Zero | Succ PeanoNat deriving (Show)

-- newtype SizedList (n :: PeanoNat) a = SizedList [a] deriving (Show)

data SizedList (n :: PeanoNat) a where
  Nil :: SizedList 'Zero a
  Cons :: a -> SizedList n a -> SizedList ('Succ n) a

type Plus2 (n :: PeanoNat) = 'Succ ('Succ n)

type family Pred (n :: PeanoNat) :: PeanoNat

type instance Pred 'Zero = 'Zero

type instance Pred ('Succ m) = m

type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat

type instance Add 'Zero m = m

type instance Add ('Succ n') m = 'Succ (Add n' m)

append :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

class CompareNat (n :: PeanoNat) (m :: PeanoNat) where
  compareNat :: Proxy n -> Proxy m -> Ordering

instance CompareNat 'Zero 'Zero where
  compareNat _ _ = EQ

instance CompareNat 'Zero ('Succ m) where
  compareNat _ _ = LT

instance CompareNat ('Succ n) 'Zero where
  compareNat _ _ = GT

instance CompareNat n m => CompareNat ('Succ n) ('Succ m) where
  compareNat _ _ = compareNat (Proxy :: Proxy n) (Proxy :: Proxy m)

data SomeSizedList a = forall n. SomeSizedList (SizedList n a)

filter :: (a -> Bool) -> SizedList n a -> SomeSizedList a
filter f Nil = SomeSizedList Nil
filter f (Cons x xs)
  | f x = case filter f xs of
      SomeSizedList ys -> SomeSizedList (Cons x ys)
  | otherwise = filter f xs

-- reverse :: SizedList n a -> SizedList n a
-- reverse xs = revAppend Nil xs
--
-- revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
-- revAppend acc Nil = acc
-- revAppend acc (Cons x xs) = revAppend (Cons x acc) xs

type Compare :: PeanoNat -> PeanoNat -> Ordering
type family Compare n m

type instance Compare 'Zero 'Zero = 'EQ

type instance Compare 'Zero ('Succ _) = 'LT

type instance Compare ('Succ _) 'Zero = 'GT

type instance Compare ('Succ n) ('Succ m) = Compare n m

class Index (i :: PeanoNat) where
  index :: Compare i n ~ 'LT => Proxy i -> SizedList n a -> a

instance Index 'Zero where
  index _ (Cons x _) = x

instance Index j => Index ('Succ j) where
  index _ (Cons _ xs) = index (Proxy :: Proxy j) xs

data SPeanoNat (n :: PeanoNat) where
  SZero :: SPeanoNat 'Zero
  SSucc :: SPeanoNat n -> SPeanoNat ('Succ n)

sAdd :: SPeanoNat n -> SPeanoNat m -> SPeanoNat (Add n m)
sAdd SZero m = m
sAdd (SSucc n') m = SSucc (sAdd n' m)

data SBool (x :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

data SOrdering (x :: Ordering) where
  SLT :: SOrdering 'LT
  SEQ :: SOrdering 'EQ
  SGT :: SOrdering 'GT

sCompare :: SPeanoNat n -> SPeanoNat m -> SOrdering (Compare n m)
sCompare SZero SZero = SEQ
sCompare SZero (SSucc _) = SLT
sCompare (SSucc _) SZero = SGT
sCompare (SSucc n) (SSucc m) = sCompare n m

instance Show (SOrdering x) where
  show SLT = "SLT"
  show SEQ = "SEQ"
  show SGT = "SGT"

class PeanoNatI (n :: PeanoNat) where
  singPeanoNat :: SPeanoNat n

instance PeanoNatI 'Zero where
  singPeanoNat = SZero

instance PeanoNatI n => PeanoNatI ('Succ n) where
  singPeanoNat = SSucc singPeanoNat

class BoolI (x :: Bool) where
  singBool :: SBool x

instance BoolI 'True where
  singBool = STrue

instance BoolI 'False where
  singBool = SFalse

class OrderingI (x :: Ordering) where
  singOrdering :: SOrdering x

instance OrderingI 'LT where
  singOrdering = SLT

instance OrderingI 'GT where
  singOrdering = SGT

instance OrderingI 'EQ where
  singOrdering = SEQ

sPeanoNatToInteger :: SPeanoNat n -> Integer
sPeanoNatToInteger SZero = 0
sPeanoNatToInteger (SSucc n') = 1 + sPeanoNatToInteger n'

peanoNatToInteger :: forall n. PeanoNatI n => Proxy n -> Integer
peanoNatToInteger _ = sPeanoNatToInteger (singPeanoNat :: SPeanoNat n)

data PeanoNatInstance (n :: PeanoNat) where
  PeanoNatInstance :: PeanoNatI n => PeanoNatInstance n

peanoNatInstance :: SPeanoNat n -> PeanoNatInstance n
peanoNatInstance SZero = PeanoNatInstance
peanoNatInstance (SSucc n') = case peanoNatInstance n' of
  PeanoNatInstance -> PeanoNatInstance

data SomePeanoNat where
  SomePeanoNat :: forall (n :: PeanoNat). SPeanoNat n -> SomePeanoNat

somePeanoNat :: PeanoNat -> SomePeanoNat
somePeanoNat Zero = SomePeanoNat SZero
somePeanoNat (Succ n) = case somePeanoNat n of
  SomePeanoNat n -> SomePeanoNat (SSucc n)

peanoNatToInteger' :: PeanoNat -> Integer
peanoNatToInteger' x = case somePeanoNat x of
  SomePeanoNat (s :: SPeanoNat n) ->
    case peanoNatInstance s of
      PeanoNatInstance ->
        peanoNatToInteger (Proxy :: Proxy n)

rightZero :: SPeanoNat n -> Add n 'Zero :~: n
rightZero SZero = Refl
rightZero (SSucc s) =
  case rightZero s of
    Refl ->
      Refl

rightSucc :: SPeanoNat n -> Proxy m -> Add n ('Succ m) :~: 'Succ (Add n m)
rightSucc SZero _ = Refl
rightSucc (SSucc s) proxy = case rightSucc s proxy of
  Refl -> Refl

sizedLength :: SizedList n a -> SPeanoNat n
sizedLength Nil = SZero
sizedLength (Cons _ xs) = SSucc (sizedLength xs)

revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil = case rightZero (sizedLength acc) of
  Refl ->
    acc
revAppend acc (Cons x (xs :: SizedList m' a)) =
  case rightSucc (sizedLength acc) (Proxy :: Proxy m') of
    Refl ->
      revAppend (Cons x acc) xs

main :: IO ()
main = do
  print $ sCompare (SSucc SZero) SZero
  print $ peanoNatToInteger' (Succ (Succ Zero))
