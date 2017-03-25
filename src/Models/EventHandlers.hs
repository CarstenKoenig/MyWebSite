{-# LANGUAGE OverloadedStrings, GADTs #-}

module Models.EventHandlers
  (EventHandler (..)
  , executeHandlerQuery
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class(liftIO)
import qualified Data.Char as C
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Postgresql
import Models.Database (Query)
import qualified Models.Database as DB
import Models.Events


data EventHandler =
  EventHandler
  { handlerName :: Text
  , handleEvent :: Event SiteEvent -> Query ()
  }


executeHandlerQuery :: EventHandler -> Query ()
executeHandlerQuery handler = do
  lastNr <- lastSeenHandlerEventNumber (handlerName handler)
  seenNr <- iterateOverEvents lastNr (handleEvent handler)
  markHandeledEventNumber (handlerName handler) seenNr


foldEvents :: EventHandler -> [Event SiteEvent] -> Query ()
foldEvents _ [] = return ()
foldEvents handler evs = do
  forM_ evs (handleEvent handler)
  let seenNr = maximum . map eventNr $ evs
  markHandeledEventNumber (handlerName handler) seenNr

----------------------------------------------------------------------
-- DB Queries

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
