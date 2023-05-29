{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import Servant
  ( Capture,
    DeleteNoContent,
    Get,
    Handler,
    Header,
    JSON,
    NoContent,
    PostNoContent,
    PutNoContent,
    ReqBody,
    Server,
    type (:<|>) (..),
    type (:>),
  )

type UserAPI =
  Capture "userid" Int :> Get '[JSON] User
    :<|> Capture "userid" Int :> DeleteNoContent

type UserAPI2 =
  Capture "userid" Int
    :> ( Get '[JSON] User
           :<|> DeleteNoContent
       )

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

server :: Server UserAPI
server = getUser :<|> deleteUser
  where
    getUser :: Int -> Handler User
    getUser _userid = error "..."

    deleteUser :: Int -> Handler NoContent
    deleteUser _userid = error "..."

server2 :: Server UserAPI2
server2 userid = getUser userid :<|> deleteUser userid
  where
    getUser :: Int -> Handler User
    getUser = error "..."

    deleteUser :: Int -> Handler NoContent
    deleteUser _userid = error "..."

type API1 =
  "users"
    :> ( Get '[JSON] [User]
           :<|> Capture "userid" Int :> Get '[JSON] User
       )

type API2 =
  ReqBody '[JSON] User
    :> ( Get '[JSON] User
           :<|> PostNoContent
       )

type API3 =
  Header "Authorization" Token
    :> ( Get '[JSON] SecretData
           :<|> ReqBody '[JSON] SecretData :> PostNoContent
       )

newtype Token = Token ByteString

newtype SecretData = SecretData ByteString

type UsersAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> PostNoContent
    :<|> Capture "userid" Int
      :> ( Get '[JSON] User
             :<|> ReqBody '[JSON] User :> PutNoContent
             :<|> DeleteNoContent
         )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations
  where
    getUsers :: Handler [User]
    getUsers = error "..."

    newUser :: User -> Handler NoContent
    newUser = error "..."

    userOperations userid =
      viewUser userid :<|> updateUser userid :<|> deleteUser userid
      where
        viewUser :: Int -> Handler User
        viewUser = error "..."

        updateUser :: Int -> User -> Handler NoContent
        updateUser = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser = error "..."

type ProductsAPI =
  Get '[JSON] [Product]
    :<|> ReqBody '[JSON] Product :> PostNoContent
    :<|> Capture "productid" Int
      :> ( Get '[JSON] Product
             :<|> ReqBody '[JSON] Product :> PutNoContent
             :<|> DeleteNoContent
         )

newtype Product = Product {productId :: Int}

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations
  where
    getProducts :: Handler [Product]
    getProducts = error "..."

    newProduct :: Product -> Handler NoContent
    newProduct = error "..."

    productOperations productid =
      viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid
      where
        viewProduct :: Int -> Handler Product
        viewProduct = error "..."

        updateProduct :: Int -> Product -> Handler NoContent
        updateProduct = error "..."

        deleteProduct :: Int -> Handler NoContent
        deleteProduct = error "..."

type CombinedAPI =
  "users" :> UsersAPI
    :<|> "products" :> ProductsAPI

serverX :: Server CombinedAPI
serverX = usersServer :<|> productsServer

type APIFor a i =
  Get '[JSON] [a]
    :<|> ReqBody '[JSON] a :> PostNoContent
    :<|> Capture "id" i
      :> ( Get '[JSON] a
             :<|> ReqBody '[JSON] a :> PutNoContent
             :<|> DeleteNoContent
         )

serverFor ::
  Handler [a] ->
  (a -> Handler NoContent) ->
  (i -> Handler a) ->
  (i -> a -> Handler NoContent) ->
  (i -> Handler NoContent) ->
  Server (APIFor a i)
serverFor listHandler newHandler viewHandler updateHandler deleteHandler =
  listHandler :<|> newHandler :<|> (\i -> viewHandler i :<|> updateHandler i :<|> deleteHandler i)

productsServer' :: Server (APIFor Product Int)
productsServer' = serverFor undefined undefined undefined undefined undefined

main :: IO ()
main = undefined
