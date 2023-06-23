{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Identity (Identity (..))
import Data.Data (Proxy (..))
import Data.Kind (Type)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)

-- ref: https://serokell.io/blog/type-families-haskell

-- Closed type family
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type S :: (Type -> Type) -> Type
data S k = MkS (k Bool) (k Integer)

s1 :: S Maybe
s1 = MkS (Just True) Nothing

s2 :: S (Either String)
s2 = MkS (Left "hi") (Right 42)

s3 :: S Identity
s3 = MkS (Identity False) (Identity 0)

-- Pair is type synonym. Pair is also sturated and non-generative.
-- Maybe is unsaturated and generative because it is only equal to itself(means that it satisifies reflecxivity).
--
-- Equality:
--    Pair Bool ~ Pair Bool -- refrexivity
--    Pair Bool ~ Pair (Bool, Bool) -- reduction
--
-- Non-generative type must be saturated.
type Pair :: Type -> Type
type Pair a = (a, a)

type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x ': xs)

infixr 5 :&

h1 :: HList [Integer, String, Bool]
h1 = 42 :& "Hello" :& True :& HNil

h2 :: HList [Char, Bool]
h2 = 'x' :& False :& HNil

hlength :: HList xs -> Int
hlength HNil = 0
hlength (_ :& xs) = 1 + hlength xs

happend :: HList xs -> HList ys -> HList (Append xs ys)
happend HNil ys = ys
happend (x :& xs) ys = x :& happend xs ys

h3 :: HList [Integer, String, Bool, Char, Bool]
h3 = happend h1 h2

type Label :: Type -> Symbol
type family Label t

type instance Label Double = "number"

type instance Label String = "string"

type instance Label Bool = "boolean"

data MyType = MT

type instance Label MyType = "mt"

label :: forall t. KnownSymbol (Label t) => String
label = symbolVal (Proxy @(Label t))

data X = X

main :: IO ()
main = do
  print $ hlength h1
  print $ hlength h2
  print $ hlength h3
  print $ label @MyType
  print $ label @Double
  return ()
