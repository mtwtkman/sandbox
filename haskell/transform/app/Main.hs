module Main where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Time (UTCTime (..))
import Data.Time.Calendar (Day (..))

data Obj = Obj {objName :: String, objIndex :: Int, objTime :: UTCTime} deriving (Eq, Show)

data Env = Env
  { envContainer :: [Obj],
    envCurrentTime :: UTCTime
  } deriving (Show)

type AppR m = ReaderT Env IO m

obj1 :: Obj
obj1 = Obj "obj1" 1 (UTCTime (ModifiedJulianDay 1) 0)

obj2 :: Obj
obj2 = Obj "obj2" 2 (UTCTime (ModifiedJulianDay 2) 0)

pop :: Obj -> AppR Env
pop obj = do
  Env c t <- ask
  liftIO $ return (Env (filter (/= obj) c) t)

main :: IO ()
main = do
  let env = Env [obj1, obj2] (UTCTime (ModifiedJulianDay 10) 0)
  newEnv <- runReaderT (pop obj1) env
  print newEnv
