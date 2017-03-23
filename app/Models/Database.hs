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

module Models.Database
  ( initializePool
  , addEvents
  , getEvents
  , startEvents
  ) where

import Web.Spock hiding (get)

import           Config
import           Control.Monad (forM_)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Resource(ResourceT, MonadBaseControl, runResourceT)
import           Data.Aeson(encode, decodeStrict')
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Int(Int64)
import           Data.Maybe (mapMaybe)
import           Data.Pool (Pool)
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Data.Time (UTCTime, getCurrentTime)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Models.Events
import           Routes
import           Session
import           Text.Markdown (Markdown(..))
import           Control.Monad.Trans.Reader (ReaderT)

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
|]


type Query a = ReaderT SqlBackend IO a


initializePool :: AppConfig -> IO (Pool SqlBackend)
initializePool cfg = do
  pool <- runNoLoggingT $ createPostgresqlPool (appConfigDb cfg) 5
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  return pool
  

startEvents :: [SiteEvent] -> SiteAdminAction BlogId
startEvents events = do
  time <- liftIO getCurrentTime
  runSqlAction $ query time
  where
    next [] = 1
    next (x:_) = eventAggregateId (entityVal x) + 1
    json = toStrict . encode
    query time = do
      nextId <- next <$> selectList [] [Desc EventAggregateId]
      forM_ events (\ev -> insert (Event nextId (json ev) time))
      return nextId


addEvents :: BlogId -> [SiteEvent] -> SiteAdminAction ()
addEvents blogId events = do
  time <- liftIO getCurrentTime
  runSqlAction $ query time
  where
    json = toStrict . encode
    query time =
      forM_ events (\ev -> insert (Event blogId (json ev) time))


getEvents :: Maybe BlogId -> SiteAction ctx [SiteEvent]
getEvents ido = do
  let map = mapMaybe (decodeStrict' . eventJson . entityVal)
  let filter =
        case ido of
          Nothing -> []
          Just id -> [EventAggregateId ==. id]
  runSqlAction (map <$> selectList [] [Asc EventId])


runSqlAction :: (HasSpock m, SpockConn m ~ SqlBackend)
       => Query a -> m a
runSqlAction action =
  runQuery (runSqlConn action)
{-# INLINE runSqlAction #-}


runOnPool :: Pool SqlBackend -> Query a -> IO a
runOnPool pool action =
  runSqlPool action pool
{-# INLINE runOnPool #-}
