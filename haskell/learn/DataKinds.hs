{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/data_kinds.html?highlight=promotion
module DataKinds where

import Data.Kind (Type)
import GHC.Base (Symbol)

data Nat = Ze | Su Nat

-- This `Vec` accepts unintent type like `Vec Int Char`.
-- data Vec :: Type -> Type -> Type where
--
-- So it can be improved like below.
data Vec :: Type -> Nat -> Type where
  Nil :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)

-- Now this `Vec Int Char` never be allowed.

data Foo :: Type -> Type where
  MkFoo1 :: a ~ Int => Foo a
  MkFoo2 :: Show a => Foo a

-- Heterogeneous list
data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

data Tuple :: (Type, Type) -> Type where
  Tuple :: a -> b -> Tuple '(a, b)

foo0 :: HList '[]
foo0 = HNil

foo1 :: HList '[Int]
foo1 = HCons (3 :: Int) HNil

-- [Int, Bool] doesn't need tick by its unambiguously but [Int] and [] are possibly a kind or a type.
foo2 :: HList [Int, Bool]
foo2 = HCons (3 :: Int) (HCons True HNil)

data Ex :: Type where
  MkEx :: forall a. a -> Ex

type family UnEx (ex :: Ex) :: k

type instance UnEx (MkEx x) = x

type family IsTypeLit a where
  IsTypeLit Nat = 'True
  IsTypeLit Symbol = 'True
  IsTypeLit a = 'False

data T :: forall a. (IsTypeLit a ~ 'True) => a -> Type where
  MkNat :: T Ze
  MkSymbol :: T "Don't panic!"
