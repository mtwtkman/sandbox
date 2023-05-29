{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
  ( Proxy (..),
    Raw,
    Server,
    serve,
    serveDirectoryWebApp,
    type (:>),
  )

type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

server :: Server StaticAPI
server = serveDirectoryWebApp "static-files"

app :: Application
app = serve staticAPI server

main :: IO ()
main = run 8081 app
