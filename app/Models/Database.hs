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

module Models.Database
  ( initializePool
  , insertBlogPost
  , getBlogPost
  ) where

import Web.Spock hiding (get)

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Resource(ResourceT, runResourceT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Data.Pool (Pool)
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Data.Time (UTCTime)
import           Text.Markdown (Markdown(..))

import qualified Models.BlogPost as BP
import Config
import Session
import Routes

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
  

insertBlogPost :: Text -> Text -> UTCTime -> SiteAdminAction BlogId
insertBlogPost title content published =
  runSQL query
  where
    query = fromSqlKey <$> (insert $ BlogPost title content published)


getBlogPost :: BlogId -> SiteAction ctx (Maybe BP.BlogPost)
getBlogPost id =
  runSQL query
  where
    query = fmap toModel <$> get (toSqlKey id)
    toModel post =
      BP.BlogPost
         (Markdown $ LT.fromStrict $ blogPostContent post)
         (blogPostTitle post)
         (blogPostPublished post)


runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) =>
          SqlPersistT (NoLoggingT (ResourceT IO)) a -> m a
runSQL action =
    runQuery $ runResourceT . runNoLoggingT . runSqlConn action
{-# INLINE runSQL #-}
