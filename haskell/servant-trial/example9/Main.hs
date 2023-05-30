{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Servant (throwError)
import Servant.API
  ( BasicAuth,
    Get,
    JSON,
    (:<|>) ((:<|>)),
    (:>),
  )
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server
  ( BasicAuthCheck (BasicAuthCheck),
    BasicAuthResult
      ( Authorized,
        Unauthorized
      ),
    Context (EmptyContext, (:.)),
    Handler,
    Server,
    err401,
    err403,
    errBody,
    serveWithContext,
  )
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Web.Cookie (parseCookies)

-- | private data that needs protection
newtype PrivateData = PrivateData {ssshhh :: Text}
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData {somedata :: Text}
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | A user we'll grab from the database when we authenticate someone
newtype User = User {userName :: Text}
  deriving (Eq, Show)

-- | a type to wrap our public api
type PublicAPI = Get '[JSON] [PublicData]

-- | a type to wrap our private api
type PrivateAPI = Get '[JSON] PrivateData

-- | our API
type BasicAPI =
  "public" :> PublicAPI
    :<|> "private" :> BasicAuth "foo-realm" User :> PrivateAPI

-- | a value holding a proxy of our API type
basicAuthApi :: Proxy BasicAPI
basicAuthApi = Proxy

authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
          then return (Authorized (User "servant"))
          else return Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

basicAuthServer :: Server BasicAPI
basicAuthServer =
  let publicAPIHandler = return [PublicData "foo", PublicData "bar"]
      privateAPIHandler (user :: User) = return (PrivateData (userName user))
   in publicAPIHandler :<|> privateAPIHandler

main :: IO ()
main = run 8080 (serveWithContext basicAuthApi basicAuthServerContext basicAuthServer)
