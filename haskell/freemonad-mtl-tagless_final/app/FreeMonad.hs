module FreeMonad where

-- ref: https://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
data Toy b next
  = Output b next
  | Bell next
  | Done

-- same as Control.Monad.Fix which is defined in Haskell base module
newtype Cheat f = Cheat (f (Cheat f))

-- Define the type to prevent program by not using Done
data FixE f e = Fix (f (FixE f e)) | Throw e

catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix x) f = Fix (fmap (`catch` f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell next) = Bell (f next)
  fmap _ Done = Done

data IncompleteException = IncompleteException

subroutine :: FixE (Toy Char) IncompleteException
subroutine = Fix (Output 'A' (Throw IncompleteException))

-- Workaround style.
program :: FixE (Toy Char) e
program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)))
