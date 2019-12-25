{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Lib
    ( runServer
    ) where

import Network.JsonRpc.Server
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import GHC.Generics
import Control.Monad.Reader (ReaderT)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Sqlite
import Conduit
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import Data.Aeson
import Web.Scotty

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Point
  x Int
  y Int
  deriving Show Generic
|]

instance ToJSON Point

port = 3000

runServer :: IO ()
runServer = do
  conn <- open ":memory:"
  backend <- wrapConnection conn (\_ _ _ _ -> pure ())
  flip runSqlPersistM backend $ runMigration migrateAll
  scotty port $
    post "/api/v1" $ do
      requestBody <- body
      response <- lift $ flip runSqlPersistM backend $ call methods requestBody
      setHeader "content-type" "application/json"
      text . TL.fromStrict . decodeUtf8 . BL.toStrict $ fromMaybe "" response

type Server = ReaderT SqlBackend (NoLoggingT (ResourceT IO))

methods :: [Method Server]
methods = [addPoint, getAllPoints]

addPoint = toMethod "add_point" f (Required "x" :+: Required "y" :+: ())
  where f :: Int -> Int -> RpcResult Server Value
        f x y = do
          id <- lift . insert $ Point x y
          pure . toJSON $ keyToValues id

getAllPoints = toMethod "get_all_points" f ()
  where f :: RpcResult Server Value
        f = do
          points <- lift allPoints
          pure . toJSON . toPersistValue $ points

        allPoints :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [Entity Point]
        allPoints = selectList [] []
