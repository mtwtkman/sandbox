{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Data (Proxy (..))
import Data.Kind (Type)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal)

type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

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
