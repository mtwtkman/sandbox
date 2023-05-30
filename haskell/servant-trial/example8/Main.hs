{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client

data Position = Position
  { xCord :: Int,
    yCord :: Int
  }
  deriving (Generic, Show)

instance FromJSON Position

newtype HelloMessage = HelloMessage {msg :: String} deriving (Generic, Show)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String,
    clientEmail :: String,
    clientAge :: Int,
    clientInterestedIn :: [String]
  }
  deriving (Generic, Show)

instance ToJSON ClientInfo

data Email = Email
  { from :: String,
    to :: String,
    subject :: String,
    body :: String
  }
  deriving (Generic, Show)

instance FromJSON Email

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

api :: Proxy API
api = Proxy

position :: Int -> Int -> ClientM Position
hello :: Maybe [Char] -> ClientM HelloMessage
marketing :: ClientInfo -> ClientM Email
position :<|> hello :<|> marketing = client api

type API' = API :<|> EmptyAPI

api' :: Proxy API'
api' = Proxy

position' :: Int -> Int -> ClientM Position
hello' :: Maybe [Char] -> ClientM HelloMessage
marketing' :: ClientInfo -> ClientM Email
(position' :<|> hello' :<|> marketing') :<|> EmptyClient = client api'

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, message, em)

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error:" <> show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em

main :: IO ()
main = run
