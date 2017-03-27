{-# LANGUAGE OverloadedStrings, RankNTypes, GADTs, FlexibleContexts #-}

module Models.Events.Database
  ( toHandlerQuery
  , getEvents
  , forwardEventHandlers
  , newFromEvents
  , appendEvents
  )
where

import Control.Monad(fail, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import Data.Aeson (FromJSON(..), encode, decodeStrict')
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.List (foldl', (\\))
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Pool (Pool)
import Data.Text(Text)
import Data.Time(UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, toSqlKey, fromSqlKey)
import Models.Database (Query)
import qualified Models.Database as DB
import Models.Events.Types


toHandlerQuery :: [EventHandler] -> EventQuery a -> Query a
toHandlerQuery handlers eq = do
  (res, evs) <- runWriterT eq
  runHandlers evs
  return res
  where
    runHandlers evs =
      sequence_ [ handleEvent h ev | h <- handlers, ev <- evs ]


forwardEventHandlers :: EventHandler -> Query ()
forwardEventHandlers handler = do
  lastNr <- lastSeenHandlerEventNumber (handlerName handler)
  seenNr <- iterateOverEvents lastNr (handleEvent handler)
  markHandeledEventNumber (handlerName handler) seenNr


getEvents :: Maybe Int64 -> Query [Event SiteEvent]
getEvents ido = do
  let filter =
        case ido of
          Nothing -> []
          Just id -> [ DB.EventAggregateId ==. id]
  mapMaybe mapEvent <$> selectList filter [Asc DB.EventId]


newFromEvents :: [SiteEvent] -> EventQuery Int64
newFromEvents events = do
  time <- liftIO getCurrentTime
  nextId <- next <$> lift (selectList [] [Desc DB.EventAggregateId])
  nrs <- lift $ forM events (\ev -> insert (DB.Event nextId (json ev) time))
  tell $ map (wrap time nextId) $ zip nrs events
  return nextId
  where
    next [] = 1
    next (x:_) = DB.eventAggregateId (entityVal x) + 1
    json = toStrict . encode
    wrap time nextId (nr, ev) =
      Event ev (fromSqlKey nr) nextId time
  

appendEvents :: Int64 -> [SiteEvent] -> EventQuery ()
appendEvents blogId events = do
  time <- liftIO getCurrentTime
  nrs <- lift $ forM events (\ev -> insert (DB.Event blogId (json ev) time))
  tell $ map (wrap time) $ zip nrs events
  where
    next [] = 1
    next (x:_) = DB.eventAggregateId (entityVal x) + 1
    json = toStrict . encode
    wrap time (nr, ev) =
      Event ev (fromSqlKey nr) blogId time



----------------------------------------------------------------------


lastSeenHandlerEventNumber :: Text -> Query Int64
lastSeenHandlerEventNumber name =
  maybe 0 DB.eventHandlerSeenNumber <$> get (DB.EventHandlerKey name)


markHandeledEventNumber :: Text -> Int64 -> Query ()
markHandeledEventNumber name evNr = do
  now <- liftIO getCurrentTime
  found <- get (DB.EventHandlerKey name)
  case found of
    Nothing ->
      insertKey (DB.EventHandlerKey name)
                (DB.EventHandler name evNr Nothing now)
    Just _ ->
      update (DB.EventHandlerKey name)
             [ DB.EventHandlerSeenNumber =. evNr
             , DB.EventHandlerUpdated =. now ]



iterateOverEvents :: Int64 -> (Event SiteEvent -> Query ())
                  -> Query Int64
iterateOverEvents lastSeen action = do
  (seenNr, evs) <- loadEventsAfter lastSeen
  forM_ evs action
  return seenNr


loadEventsAfter :: Int64 -> Query (Int64, [Event SiteEvent])
loadEventsAfter afterNr = do
  evs <- selectList [DB.EventId >. toSqlKey afterNr]
                    [Asc DB.EventId]
  return $ if null evs
    then (afterNr, [])
    else (maxNr evs, wrap evs)
  where
    maxNr = maximum . map (fromSqlKey . entityKey)
    wrap = mapMaybe mapEvent


mapEvent :: FromJSON ev => Entity DB.Event -> Maybe (Event ev)
mapEvent row = do
  val <- getJson
  return $ Event val nr id added
  where
    evVal = entityVal row
    nr = fromSqlKey $ entityKey row
    id = DB.eventAggregateId evVal
    added = DB.eventAdded evVal
    getJson  = decodeStrict' $ DB.eventJson evVal
