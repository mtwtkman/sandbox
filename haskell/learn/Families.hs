{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.Identity (Identity (..))
import Data.Binary (Word8)
import Data.ByteString
import Data.ByteString qualified as ByteString
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.Maybe (maybeToList)
import Data.Type.Bool (Not)
import Data.Typeable (Typeable, typeRep)
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

type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b where
  MaybeIf 'True = Maybe
  MaybeIf 'False = Identity

data PlayerInfo b = MkPlayerInfo {name :: MaybeIf b String, score :: MaybeIf b Integer}

dbReadPlayerInfo :: IO (PlayerInfo False)
dbReadPlayerInfo = undefined

dbUpdatePlayerInfo :: PlayerInfo True -> IO ()
dbUpdatePlayerInfo = undefined

-- When `MaybeIf` arity is 2, `AuxInfo` can't compile by ariby violation.
newtype AuxInfo b = MkAuxInfo (S (MaybeIf b))

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

class Container a where
  type Elem a
  type Elem x = Unwrap x
  elements :: a -> [Elem a]

instance Container [a] where
  elements = id

instance Container (Maybe a) where
  elements = maybeToList

instance Container ByteString where
  type Elem ByteString = Word8
  elements = ByteString.unpack

type family Unwrap x where
  Unwrap (f a) = a

data ByteArray = ByteArray

data family Vector a

newtype instance Vector () = VUnit Int

newtype instance Vector Word8 = VBytes ByteArray

data instance Vector (a, b) = VPair !(Vector a) !(Vector b)

type family VectorF a

type instance VectorF () = VectorUnit

newtype VectorUnit = VUnitF Int

type instance VectorF Word8 = VectorWord8

newtype VectorWord8 = VBytesF ByteArray

type instance VectorF (a, b) = VectorPair a b

data VectorPair a b = VPairF (VectorF a) (VectorF b)

main :: IO ()
main = do
  print $ hlength h1
  print $ hlength h2
  print $ hlength h3
  print $ label @MyType
  print $ label @Double
  return ()
