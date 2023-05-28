{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Except ( Monad(return) )
import Control.Monad.Reader ()
import Data.Aeson ( ToJSON )
import Data.Aeson.Types ()
import Data.Attoparsec.ByteString ()
import Data.List ()
import Data.Maybe ()
import Data.String.Conversions ()
import Data.Time.Calendar ()
import GHC.Generics ( Generic )
import Lucid ( renderBS, table_, td_, th_, tr_, Html, ToHtml(..) )
import Network.HTTP.Media ((//), (/:))
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Prelude.Compat ( ($), Foldable(foldMap), String, IO, (.) )
import Servant
    ( Proxy(..),
      serve,
      Accept(contentType),
      JSON,
      MimeRender(..),
      type (:>),
      Get,
      Server )
import System.Directory ()
import Text.Blaze ( ToMarkup )
import Text.Blaze.Html ( toHtml, Html )
import Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import Prelude ()

data HTMLLucid

instance Accept HTMLLucid where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
  mimeRender _ = renderBS . Lucid.toHtml

instance MimeRender HTMLLucid (Lucid.Html a) where
  mimeRender _ = renderBS

data HTMLBlaze

instance Accept HTMLBlaze where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender HTMLBlaze a where
  mimeRender _ = renderHtml . Text.Blaze.Html.toHtml

instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
  mimeRender _ = renderHtml

type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

data Person = Person
  { firstName :: String,
    lastName :: String
  }
  deriving (Generic)

instance ToJSON Person

instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (Lucid.toHtml $ firstName person)
      td_ (Lucid.toHtml $ lastName person)

  toHtmlRaw = Lucid.toHtml

instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"
    foldMap Lucid.toHtml persons
  toHtmlRaw = Lucid.toHtml

people :: [Person]
people =
  [ Person "Isaac" "Newton",
    Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server :: Server PersonAPI
server = return people

app :: Application
app = serve personAPI server

main :: IO ()
main = run 8081 app
