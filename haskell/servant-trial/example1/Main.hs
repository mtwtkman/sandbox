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
  ( Get,
    JSON,
    Proxy (..),
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )

type UserAPI1 = "users" :> Get '[JSON] [User]

type UserAPI2 =
  "users" :> Get '[JSON] [User]
    :<|> "albert" :> Get '[JSON] User
    :<|> "isaac" :> Get '[JSON] User

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1),
    User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  ]

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server1 :: Server UserAPI1
server1 = return users1

server2 :: Server UserAPI2
server2 =
  return users2
    :<|> return albert
    :<|> return isaac

userAPI1 :: Proxy UserAPI1
userAPI1 = Proxy

app1 :: Application
app1 = serve userAPI1 server1

app2 :: Application
app2 = serve (Proxy :: Proxy UserAPI2) server2

main :: IO ()
main = run 8081 app2
