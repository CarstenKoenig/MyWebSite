{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes #-}
module Config
 (AppConfig(..)
 , defaultAppConfig
 ) where

import Web.Spock

import Database.Persist.Postgresql(ConnectionString)

import Data.Text (Text)

data AppConfig
  = AppConfig
    { appConfigDb :: ConnectionString
    }


defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig "host=localhost port=5432 user=blog dbname=webapp password=blog"
