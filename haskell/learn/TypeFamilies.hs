{-# LANGUAGE TypeFamilies #-}

module TypeFamilies where
import Data.Proxy (Proxy)
-- ref: https://zenn.dev/mod_poppo/books/haskell-type-level-programming
data Expr a
  = Const Int
  | Add (Expr Int) (Expr Int)
  | Equal (Expr Int) (Expr Int)
  | IfThenElse (Expr Bool) (Expr a) (Expr a)
  deriving (Show)

mkConst :: Int -> Expr Int
mkConst = Const

mkAdd :: Expr Int -> Expr Int -> Expr Int
mkAdd = Add

mkEqual :: Expr Int -> Expr Int -> Expr Bool
mkEqual = Equal

maxBoundAsInteger :: forall a. (Integral a, Bounded a) => Proxy a -> Integer
maxBoundAsInteger _proxy = toInteger (maxBound :: a)



