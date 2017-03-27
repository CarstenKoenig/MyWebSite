{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}

module Utils.Database where

import           Config
import           Control.Monad.Catch (MonadCatch, catch)
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import           Data.Pool (Pool)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Database.PostgreSQL.Simple (SqlError(..))
import           Models.Database
import           Models.Events
import           Routes
import           Session
import           Text.Markdown (Markdown(..))
import           Web.Spock hiding (get)

----------------------------------------------------------------------
-- SQL execution

type SqlResult a = Either SqlError a


initializePool :: AppConfig -> IO (Pool SqlBackend)
initializePool cfg = do
  pool <- runNoLoggingT $ createPostgresqlPool (appConfigDb cfg) 5
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  return pool


runEventAction :: (Monad m, HasSpock m
                  , SpockConn m ~ SqlBackend
                  , SpockState m ~ SiteState)
               => EventQuery a -> m (SqlResult a)
runEventAction query = do
  hds <- handlers . appConfig <$> getState
  runSqlAction (toHandlerQuery hds query)


runSqlAction :: (HasSpock m, SpockConn m ~ SqlBackend)
       => Query a -> m (SqlResult a)
runSqlAction action =
  runQuery runSql
  where
    runSql backend =
      catch (Right <$> runSqlConn action backend)
            (return . Left)
{-# INLINE runSqlAction #-}


runOnPool :: Pool SqlBackend -> Query a -> IO a
runOnPool pool action =
  runSqlPool action pool
{-# INLINE runOnPool #-}
