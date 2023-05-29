{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Capture,
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    QueryParam,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCord :: Int,
    yCord :: Int
  }
  deriving (Generic)

instance ToJSON Position

newtype HelloMessage = HelloMessage {msg :: String} deriving (Generic)

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String,
    clientEmail :: String,
    clientAge :: Int,
    clientInterestedIn :: [String]
  }
  deriving (Generic)

instance FromJSON ClientInfo

instance ToJSON ClientInfo

data Email = Email
  { from :: String,
    to :: String,
    subject :: String,
    body :: String
  }
  deriving (Generic)

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " <> clientName c <> ", we miss you!"
    body' =
      "Hi "
        <> clientName c
        <> ",\n\n"
        <> "Since you've recently turned"
        <> show (clientAge c)
        <> ", have you checked out our latest "
        <> intercalate ", " (clientInterestedIn c)
        <> " products? Give us a visit!"

server :: Server API
server =
  position
    :<|> hello
    :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x y)

    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous coward"
      Just n -> "Hello, " <> n

    marketing :: ClientInfo -> Handler Email
    marketing = return . emailForClient

app :: Network.Wai.Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = run 8081 app
