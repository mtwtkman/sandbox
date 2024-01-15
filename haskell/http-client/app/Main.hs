{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (ToJSON, Value)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Req (
  Option,
  POST (POST),
  ReqBodyJson (ReqBodyJson),
  Scheme (Https),
  defaultHttpConfig,
  header,
  jsonResponse,
  req,
  responseBody,
  runReq,
  useHttpsURI,
 )
import Text.URI (mkURI)

import Data.Maybe (fromJust)
import System.Environment (getEnv)

fetchChannelId :: IO String
fetchChannelId = getEnv "CHANNEL"

tokenName :: Text
tokenName = "TOKEN"

fetchToken :: IO B.ByteString
fetchToken = do
  token <- getEnv (unpack tokenName)
  return (C.pack token)

apiUrl :: Text
apiUrl = "https://slack.com/api/"

authHeader :: B.ByteString -> Option Https
authHeader token = header "Authorization" ("Bearer " <> token)

postReq :: (ToJSON a) => B.ByteString -> Text -> a -> IO Value
postReq token endpoint payload = runReq defaultHttpConfig $ do
  uri <- mkURI (apiUrl <> endpoint)
  let (url, options) = fromJust (useHttpsURI uri)
  r <-
    req POST url (ReqBodyJson payload) jsonResponse $
      authHeader token
        <> options
  return $ responseBody r

newtype ConversationsHistoryPayload = ConversationsHistoryPayload
  { channel :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON ConversationsHistoryPayload

postConversationsHistory :: B.ByteString -> ConversationsHistoryPayload -> IO Value
postConversationsHistory token = postReq token "conversations.history"

main :: IO ()
main = do
  token <- fetchToken
  channelId <- fetchChannelId
  resp <- postConversationsHistory token (ConversationsHistoryPayload channelId)
  print resp
