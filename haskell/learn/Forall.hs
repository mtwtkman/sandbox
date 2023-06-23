module Main where

-- ref: https://stackoverflow.com/a/3071932

--ScopedTypeVariables
foob :: forall a b. (b -> b) -> b -> (a -> b) -> Maybe a -> b
foob postProcess onNothin onJust mval =
  postProcess val
  where
    val :: b -- This `b` is the same one as function signature.
    val = maybe onNothin onJust mval

-- RankNTypes
liftTup :: (forall x. x -> f x) -> (a, b) -> (f a, f b) -- This `x` distincts (t :: a) and (v :: b)
liftTup liftFunc (t, v) = (liftFunc t, liftFunc v)

-- ExistentialQuantification
data EQList = forall a. EQList [a]
eqListLen :: EQList -> Int
eqListLen (EQList x) = length x

main :: IO ()
main = return ()
