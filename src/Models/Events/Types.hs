{-# LANGUAGE OverloadedStrings, DeriveGeneric, RankNTypes, GADTs, FlexibleContexts #-}

module Models.Events.Types where

import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import Data.Aeson (ToJSON(..),FromJSON(..), Value(..), (.:), (.=), object, encode, decodeStrict')
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text(Text)
import Data.Time(UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Models.Database (Query)
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
