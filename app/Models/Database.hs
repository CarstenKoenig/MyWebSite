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

module Models.Database
  ( initializePool
  , addEvents
  , getEvents
  , iterateOverEvents
  , startEvents
  , Query
  , runOnPool
  , runSqlAction
  , setBlogIndexTitle
  , setBlogIndexDate
  , indexToId
  , lastSeenHandlerEventNumber
  , markHandeledEventNumber
  ) where

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
import           Models.Events
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
-- Event Handler
lastSeenHandlerEventNumber :: Text -> Query Int64
lastSeenHandlerEventNumber name =
  maybe 0 eventHandlerSeenNumber <$> get (EventHandlerKey name)


markHandeledEventNumber :: Text -> Int64 -> Query ()
markHandeledEventNumber name evNr = do
  now <- liftIO getCurrentTime
  update (EventHandlerKey name)
    [ EventHandlerSeenNumber =. evNr
    , EventHandlerUpdated =. now ]

----------------------------------------------------------------------
-- Blog Index Operations

setBlogIndexTitle :: BlogId -> Text -> Query ()
setBlogIndexTitle blogId title = do
  now <- liftIO getCurrentTime
  let (year, month, _) = toGregorian (utctDay now)
  _ <- upsert
       (BlogIndex (fromIntegral year) month title blogId) 
       [ BlogIndexTitle =. title ]
  return ()


setBlogIndexDate :: BlogId -> UTCTime -> Query ()
setBlogIndexDate blogId time = do
  let (year, month, _) = toGregorian (utctDay time)
      title = pack $ show blogId
  _ <- upsert
       (BlogIndex (fromIntegral year) month title blogId) 
       [ BlogIndexYear =. fromIntegral year
       , BlogIndexMonth =. month ]
  return ()
  


indexToId :: Int -> Int -> Text -> Query (Maybe BlogId)
indexToId year month title =
  fmap (blogIndexAggregateId . entityVal) <$> getBy (UniquePath year month title)



----------------------------------------------------------------------
-- Eventsource

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


iterateOverEvents :: Pool SqlBackend ->  Int64
                  -> (Int64 -> UTCTime -> Int64 -> SiteEvent -> Query ())
                  -> Query Int64
iterateOverEvents pool startNr action =
  let
    applyEv ev =
      let evVal = entityVal ev
      in fmap
           (action (fromSqlKey $ entityKey ev)
                   (eventAdded evVal)
                   (eventAggregateId evVal)
           ) (decodeStrict' $ eventJson evVal)
  in do
    evs <- selectList [EventId >=. toSqlKey startNr]
                      [Asc EventId]
    let acts = mapMaybe applyEv evs
    sequence_ acts
    return . maximum . map (fromSqlKey . entityKey) $ evs
   
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
