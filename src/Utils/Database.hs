{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}

module Utils.Database where

import           Web.Spock hiding (get)
import           Config
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import           Data.Pool (Pool)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Models.Database
import           Routes
import           Session
import           Text.Markdown (Markdown(..))

----------------------------------------------------------------------
-- SQL execution

initializePool :: AppConfig -> IO (Pool SqlBackend)
initializePool cfg = do
  pool <- runNoLoggingT $ createPostgresqlPool (appConfigDb cfg) 5
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  return pool


runSqlAction :: (HasSpock m, SpockConn m ~ SqlBackend)
       => Query a -> m a
runSqlAction action =
  runQuery (runSqlConn action)
{-# INLINE runSqlAction #-}


runOnPool :: Pool SqlBackend -> Query a -> IO a
runOnPool pool action =
  runSqlPool action pool
{-# INLINE runOnPool #-}
