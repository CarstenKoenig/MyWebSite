{-# LANGUAGE OverloadedStrings #-}

module Models.EventHandlers
  (EventHandler (..)
  , executeHandler
  )
where

import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Postgresql
import Models.Database
import Models.Events


data EventHandler =
  EventHandler
  { handlerName :: Text
  , handleEvent :: Int64 -> UTCTime -> Int64 -> SiteEvent -> Query ()
  }


executeHandler :: Pool SqlBackend -> EventHandler -> IO ()
executeHandler pool handler =
  let query =  do
        nextEv <- (+1) <$> lastSeenHandlerEventNumber (handlerName handler)
        lastNr <- iterateOverEvents pool nextEv (handleEvent handler)
        markHandeledEventNumber (handlerName handler) lastNr
  in runOnPool pool query


blogIndexHandler :: EventHandler
blogIndexHandler = EventHandler name handle
  where
    name = "BlogIndex"
    
    handle evNr _ aggId event = do
      query aggId event
      markHandeledEventNumber name evNr
      
    query aggId (BlogEntry (TitleSet title)) =
      setBlogIndexTitle aggId title
    query aggId (BlogEntry (PublishedAt time)) =
      setBlogIndexDate aggId time
    query _ _ =
      return ()
