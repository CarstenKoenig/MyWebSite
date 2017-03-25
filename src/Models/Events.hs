{-# LANGUAGE OverloadedStrings, DeriveGeneric, RankNTypes, GADTs #-}

module Models.Events where

import Control.Monad(fail, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..),FromJSON(..), Value(..), (.:), (.=), object, encode, decodeStrict')
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Pool (Pool)
import Data.Text(Text)
import Data.Time(UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend, toSqlKey, fromSqlKey)
import GHC.Generics (Generic)
import Models.Database (Query)
import qualified Models.Database as DB
import Routes
import Session
import Text.Markdown(Markdown(..))

newtype SiteEvent
  = BlogEntry BlogEntryEvent
  deriving (Show, Eq, Generic)

instance ToJSON SiteEvent
instance FromJSON SiteEvent


data BlogEntryEvent
  = ContentSet Markdown
  | TitleSet Text
  | PublishedAt UTCTime
  deriving (Show, Eq, Generic)


instance ToJSON BlogEntryEvent
instance FromJSON BlogEntryEvent

instance FromJSON Markdown where
  parseJSON (Object v) =
    Markdown <$> v .: "markdown"
  parseJSON _ = fail "expected a markdown object"

instance ToJSON Markdown where
  toJSON (Markdown text) = object [ "markdown" .= text ]


----------------------------------------------------------------------
-- DB Queries

startEvents :: [SiteEvent] -> SiteAdminAction BlogId
startEvents events = do
  time <- liftIO getCurrentTime
  DB.runSqlAction $ query time
  where
    next [] = 1
    next (x:_) = DB.eventAggregateId (entityVal x) + 1
    json = toStrict . encode
    query time = do
      nextId <- next <$> selectList [] [Desc DB.EventAggregateId]
      forM_ events (\ev -> insert (DB.Event nextId (json ev) time))
      return nextId


addEvents :: BlogId -> [SiteEvent] -> SiteAdminAction ()
addEvents blogId events = do
  time <- liftIO getCurrentTime
  DB.runSqlAction $ query time
  where
    json = toStrict . encode
    query time =
      forM_ events (\ev -> insert (DB.Event blogId (json ev) time))


getEvents :: Maybe BlogId -> SiteAction ctx [SiteEvent]
getEvents ido = do
  let map = mapMaybe (decodeStrict' . DB.eventJson . entityVal)
  let filter =
        case ido of
          Nothing -> []
          Just id -> [ DB.EventAggregateId ==. id]
  DB.runSqlAction (map <$> selectList filter [Asc DB.EventId])


iterateOverEvents :: Int64 -> (Int64 -> UTCTime -> Int64 -> SiteEvent -> Query ())
                  -> Query Int64
iterateOverEvents lastSeen action =
  let
    applyEv ev =
      let evVal = entityVal ev
      in fmap
           (action (fromSqlKey $ entityKey ev)
                   (DB.eventAdded evVal)
                   (DB.eventAggregateId evVal)
           ) (decodeStrict' $ DB.eventJson evVal)
  in do
    evs <- selectList [DB.EventId >=. toSqlKey (lastSeen + 1)]
                      [Asc DB.EventId]
    let acts = mapMaybe applyEv evs
    sequence_ acts
    if null evs
      then return lastSeen
      else return . maximum . map (fromSqlKey . entityKey) $ evs
