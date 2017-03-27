{-# LANGUAGE OverloadedStrings, DeriveGeneric, RankNTypes, GADTs, FlexibleContexts #-}

module Models.Events where

import Control.Monad(fail, forM_, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import Data.Aeson (ToJSON(..),FromJSON(..), Value(..), (.:), (.=), object, encode, decodeStrict')
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
import GHC.Generics (Generic)
import Models.Database (Query)
import qualified Models.Database as DB
import Models.Projections
import Routes
import Text.Markdown(Markdown(..))


data Event ev =
  Event { event       :: ev
        , eventNr     :: Int64
        , aggregateId :: Int64
        , addedAt     :: UTCTime
        } deriving (Show, Eq)


newtype SiteEvent
  = BlogEntry BlogEntryEvent
  deriving (Show, Eq, Generic)

instance ToJSON SiteEvent
instance FromJSON SiteEvent


data BlogEntryEvent
  = ContentSet Markdown
  | TitleSet Text
  | PublishedAt UTCTime
  | AddedToCategory Category
  | RemovedFromCategory Category
  deriving (Show, Eq, Generic)

instance ToJSON BlogEntryEvent
instance FromJSON BlogEntryEvent

instance FromJSON Markdown where
  parseJSON (Object v) =
    Markdown <$> v .: "markdown"
  parseJSON _ = fail "expected a markdown object"

instance ToJSON Markdown where
  toJSON (Markdown text) = object [ "markdown" .= text ]


newtype Category =
  Category { categoryName :: Text }
  deriving (Eq, Show)

instance FromJSON Category where
  parseJSON v = Category <$> parseJSON v

instance ToJSON Category where
  toJSON (Category text) = toJSON text


data EventHandler =
  EventHandler
  { handlerName :: Text
  , handleEvent :: Event SiteEvent -> Query ()
  }


type EventQuery =
  WriterT [Event SiteEvent] Query

----------------------------------------------------------------------
-- Projections

fromSite :: SiteEvent -> Maybe BlogEntryEvent
fromSite (BlogEntry ev) = Just ev


contentP =
  fromMaybe (Markdown "") <$> lastP contSet
  where
    contSet (ContentSet c) = Just c
    contSet _ = Nothing

titleP =    
  fromMaybe "" <$> lastP titleSet
  where
    titleSet (TitleSet t) = Just t
    titleSet _ = Nothing

publishedAtP now =
  fromMaybe now <$> lastP publAt
  where
    publAt (PublishedAt t) = Just t
    publAt _ = Nothing


categoriesP =
  (\\) <<* addP <<$ remP
  where
    addP = collectP catAdd
    catAdd (AddedToCategory c) = Just c
    catAdd _ = Nothing
    remP = collectP catRem
    catRem (RemovedFromCategory c) = Just c
    catRem _ = Nothing


----------------------------------------------------------------------

toHandlerQuery :: [EventHandler] -> EventQuery a -> Query a
toHandlerQuery handlers eq = do
  (res, evs) <- runWriterT eq
  runHandlers evs
  return res
  where
    runHandlers evs =
      sequence_ [ handleEvent h ev | h <- handlers, ev <- evs ]


newFromEvents :: [SiteEvent] -> EventQuery BlogId
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
  

appendEvents :: BlogId -> [SiteEvent] -> EventQuery ()
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
-- DB Queries

getEvents :: Maybe BlogId -> Query [Event SiteEvent]
getEvents ido = do
  let filter =
        case ido of
          Nothing -> []
          Just id -> [ DB.EventAggregateId ==. id]
  mapMaybe mapEvent <$> selectList filter [Asc DB.EventId]


getMaybeProjection :: Projection SiteEvent state result -> BlogId -> Query (Maybe result)
getMaybeProjection p id = do
  evs <- map event <$> getEvents (Just id)
  case evs of
    [] -> return Nothing
    _  -> return . Just $ getResult p evs


getProjection :: Projection SiteEvent state result -> BlogId -> Query result
getProjection p id = do
  evs <- map event <$> getEvents (Just id)
  return $ getResult p evs


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

----------------------------------------------------------------------


iterateOverEvents :: Int64 -> (Event SiteEvent -> Query ())
                  -> Query Int64
iterateOverEvents lastSeen action = do
  (seenNr, evs) <- loadEventsAfter lastSeen
  forM_ evs action
  return seenNr


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


forwardEventHandlers :: EventHandler -> Query ()
forwardEventHandlers handler = do
  lastNr <- lastSeenHandlerEventNumber (handlerName handler)
  seenNr <- iterateOverEvents lastNr (handleEvent handler)
  markHandeledEventNumber (handlerName handler) seenNr


foldEvents :: EventHandler -> [Event SiteEvent] -> Query ()
foldEvents _ [] = return ()
foldEvents handler evs = do
  forM_ evs (handleEvent handler)
  let seenNr = maximum . map eventNr $ evs
  markHandeledEventNumber (handlerName handler) seenNr
