{-# LANGUAGE DeriveFunctor #-}

module CooperativeThread where

import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Trans.Free (FreeF (Free, Pure), FreeT (runFreeT), liftF)
import Control.Monad.Writer (Writer, tell)
import qualified Control.Monad.Writer as W
import Data.Sequence (ViewL (EmptyL, (:<)), singleton, viewl, (<|), (|>))

-- ref: https://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html

data ThreadF next
  = Fork next next
  | Yield next
  | Done
  deriving (Functor)

type Thread = FreeT ThreadF

yield :: Monad m => Thread m ()
yield = liftF (Yield ())

done :: Monad m => Thread m r
done = liftF Done

cFork :: Monad m => Thread m Bool
cFork = liftF (Fork False True)

fork :: Monad m => Thread m a -> Thread m ()
fork thread = do
  child <- cFork
  when child $ do
    _ <- thread
    done

roundRobin :: Monad m => Thread m a -> m ()
roundRobin t = go (singleton t)
  where
    go ts = case viewl ts of
      EmptyL -> return ()
      t' :< ts' -> do
        x <- runFreeT t'
        case x of
          Free (Fork t1 t2) -> go (t1 <| (ts' |> t2))
          Free (Yield t'') -> go (ts' |> t'')
          Free Done -> go ts'
          Pure _ -> go ts'

mainThread :: Thread IO ()
mainThread = do
  liftIO $ putStrLn "Forking thread #1"
  fork thread1
  liftIO $ putStrLn "Forking thread #2"
  fork thread2

thread1 :: Thread IO ()
thread1 = forM_ ([1 .. 10] :: [Integer]) $ \i -> do
  liftIO $ print i
  yield

thread2 :: Thread IO ()
thread2 = replicateM_ 3 $ do
  liftIO $ putStrLn "Hello"
  yield

logger :: Thread (Writer [String]) ()
logger = do
  fork helper
  W.lift $ tell ["Abort"]
  yield
  W.lift $ tell ["Fail"]

helper :: Thread (Writer [String]) ()
helper = do
  W.lift $ tell ["Retry"]
  yield
  W.lift $ tell ["!"]
