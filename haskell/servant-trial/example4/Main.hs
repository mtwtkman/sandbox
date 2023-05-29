{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (liftIO),
    throwError,
  )
import Data.Aeson (ToJSON)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Application,
    Get,
    JSON,
    Proxy (..),
    Server,
    ServerError (errBody),
    err404,
    serve,
    type (:>),
  )
import System.Directory (doesFileExist)

type IOAPI = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  {content :: String}
  deriving (Generic)

instance ToJSON FileContent

server :: Server IOAPI
server = do
  exists <- Control.Monad.Except.liftIO (doesFileExist "myfile.txt")
  if exists
    then liftIO (readFile "myfile.txt") <&> FileContent
    else throwError custom404Err
  where
    custom404Err = err404 {errBody = "myfile.txt just isn't there, please leave this server alone."}

app :: Application
app = serve (Proxy :: Proxy IOAPI) server

main :: IO ()
main = run 8081 app
