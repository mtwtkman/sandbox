{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (ToJSON)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Capture,
    Get,
    Header,
    Headers,
    JSON,
    Proxy (..),
    Server,
    addHeader,
    noHeader,
    serve,
    type (:<|>) (..),
    type (:>),
  )

type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

instance ToJSON User

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 albert

type MyHeadfulHandler = "headful" :> Get '[JSON] (Headers '[Header "X-A-Bool" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = return $ addHeader True $ addHeader 1797 albert

type MyMaybeHeaderHandler = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x = return $ if x then addHeader 1797 albert else noHeader albert

type API = MyHandler :<|> MyHeadfulHandler :<|> MyMaybeHeaderHandler

server :: Server API
server = myHandler :<|> myHeadfulHandler :<|> myMaybeHeaderHandler

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = run 8081 app
