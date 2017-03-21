{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Database
  ( initializePool
  ) where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Pool (Pool)
import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Text.Markdown (Markdown)

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
BlogPost
    title Text
    content Text
    published UTCTime
    deriving Show
|]


initializePool :: AppConfig -> IO (Pool SqlBackend)
initializePool cfg = do
  pool <- runNoLoggingT $ createPostgresqlPool (appConfigDb cfg) 5
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  return pool
  
