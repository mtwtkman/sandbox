module Main where

import Network.HTTP.Req

import System.Environment (getEnv)

tokenName :: String
tokenName = "SLACK_API_TOKEN"

fetchToken :: IO String
fetchToken = getEnv tokenName

apiUrl :: String
apiUrl = "https://slack.com/api/"

main :: IO ()
main = do
  token <- fetchToken
  print token
