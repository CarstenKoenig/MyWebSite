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

module Models.Database where

import Web.Spock hiding (get)

import           Control.Monad.Trans.Reader (ReaderT)
import           Data.ByteString (ByteString)
import           Data.Int(Int64)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
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
    aggregateId Int64
    UniquePath year month title
    Primary aggregateId

BlogCategoryMap
    aggregateId Int64
    category Text
    Primary aggregateId category
|]


type Query = ReaderT SqlBackend IO
