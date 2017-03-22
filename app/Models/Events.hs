{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Models.Events where

import Control.Monad(fail)
import Data.Aeson (ToJSON(..),FromJSON(..), Value(..), (.:), (.=), object)
import Data.Text(Text)
import Data.Time(UTCTime)
import GHC.Generics (Generic)
import Text.Markdown(Markdown(..))

data SiteEvent
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
