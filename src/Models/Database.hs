{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}

module Models.Database where

import Web.Spock hiding (get)

import           Config
import           Control.Monad (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Resource(ResourceT, MonadBaseControl, runResourceT)
import           Data.Aeson(encode, decodeStrict')
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Int(Int64)
import           Data.Maybe (mapMaybe)
import           Data.Pool (Pool)
import           Data.Text (Text, pack)
import qualified Data.Text.Lazy as LT
import           Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Routes
import           Session
import           Text.Markdown (Markdown(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Event
    aggregateId Int64
    json ByteString
    added UTCTime

EventHandler
    name Text
    seenNumber Int64
    error Text Maybe Text default=NULL
    updated UTCTime default=now()
    Primary name

BlogIndex
    year Int
    month Int
    title Text
    aggregateId BlogId
    UniquePath year month title
    Primary aggregateId
|]


type Query a = ReaderT SqlBackend IO a

----------------------------------------------------------------------
-- SQL execution

initializePool :: AppConfig -> IO (Pool SqlBackend)
initializePool cfg = do
  pool <- runNoLoggingT $ createPostgresqlPool (appConfigDb cfg) 5
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  return pool


runSqlAction :: (HasSpock m, SpockConn m ~ SqlBackend)
       => Query a -> m a
runSqlAction action =
  runQuery (runSqlConn action)
{-# INLINE runSqlAction #-}


runOnPool :: Pool SqlBackend -> Query a -> IO a
runOnPool pool action =
  runSqlPool action pool
{-# INLINE runOnPool #-}
