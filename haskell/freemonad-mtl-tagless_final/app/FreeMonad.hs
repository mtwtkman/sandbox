module FreeMonad where

import Control.Exception (throwIO)

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

-- Workaround style.
-- This style can't avoit for using exceptional value as ordinary value.
-- subroutine :: FixE (Toy Char) IncompleteException
-- subroutine = Fix (Output 'A' (Throw IncompleteException))
-- program :: FixE (Toy Char) e
-- program = subroutine `catch` (\_ -> Fix (Bell (Fix Done)))

-- Introduce Free monad.
data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Functor (Free f) where
  fmap f (Free x) = Free (fmap (fmap f) x)
  fmap f (Pure r) = Pure (f r)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure r = pure (f r)
  Pure f <*> Free x = fmap f (Free x)
  Free f <*> Pure r = Free (fmap (fmap (\f' -> f' r)) f)
  Free f <*> Free x = Free (fmap (<*> Free x) f)

instance Functor f => Monad (Free f) where
  return = pure
  (Free x) >>= f = Free (fmap (>>= f) x)
  (Pure r) >>= f = f r

liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)

output :: a -> Free (Toy a) ()
output x = liftF (Output x ())

bell :: Free (Toy a) ()
bell = liftF (Bell ())

done :: Free (Toy a) r
done = liftF Done

subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: (Show r) => Free (Toy Char) r
program = do
  subroutine
  bell
  done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) = "output " <> show a <> "\n" <> showProgram x
showProgram (Free (Bell x)) = "bell\n" <> showProgram x
showProgram (Free Done) = "done\n"
showProgram (Pure r) = "return " <> show r <> "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showProgram

ringBell :: IO ()
ringBell = print "ring ring"

interpret :: (Show b) => Free (Toy b) r -> IO ()
interpret (Free (Output b x)) = print b >> interpret x
interpret (Free (Bell x)) = ringBell >> interpret x
interpret (Free Done) = return ()
interpret (Pure _) = throwIO (userError "Improper termination")

-- Concurrent by using Free monad structure
data Thread m r = Atomic (m (Thread m r)) | Return r

atomic :: (Monad m) => m a -> Thread m a
atomic = Atomic . fmap Return

instance Functor f => Functor (Thread f) where
  fmap f (Atomic x) = Atomic (fmap (fmap f) x)
  fmap f (Return r) = Return (f r)

instance Functor f => Applicative (Thread f) where
  pure = Return
  Return f <*> Return r = Return (f r)
  Return f <*> Atomic x = fmap f (Atomic x)
  Atomic f <*> Return r = Atomic (fmap (fmap (\f' -> f' r)) f)
  Atomic f <*> Atomic x = Atomic (fmap (<*> Atomic x) f)

instance Monad m => Monad (Thread m) where
  return = pure
  (Atomic m) >>= f = Atomic (fmap (>>= f) m)
  (Return r) >>= f = f r

thread1 :: Thread IO ()
thread1 = do
  atomic $ print (1 :: Integer)
  atomic $ print (2 :: Integer)

thread2 :: Thread IO ()
thread2 = do
  str <- atomic getLine
  atomic $ putStrLn str

interleave :: Monad m => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
  next1 <- atomic m1
  next2 <- atomic m2
  interleave next1 next2
interleave t1 (Return _) = t1
interleave (Return _) t2 = t2

runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r
