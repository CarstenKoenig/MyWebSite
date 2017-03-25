{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}


module Models.BlogCategory
  ( blogCategoryHandler
  )
where

import Control.Monad.IO.Class(liftIO)
import qualified Data.Char as C
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import Database.Persist
import Database.Persist.Postgresql
import Models.Database (Query)
import qualified Models.Database as DB
import Models.Events
import Routes


blogCategoryHandler :: EventHandler
blogCategoryHandler = EventHandler name handle
  where
    name = "BlogCategoryMap"
    handle ev =
      query (aggregateId ev) (event ev)
    query aggId (BlogEntry (AddedToCategory catId)) =
      addToCategory aggId catId
    query aggId (BlogEntry (RemovedFromCategory catId)) =
      removeFromCategory aggId catId
    query _ _ =
      return ()


----------------------------------------------------------------------
-- DB Queries

addToCategory :: BlogId -> Category -> Query ()
addToCategory blogId (Category cat) =
  repsert (DB.BlogCategoryMapKey blogId cat) (DB.BlogCategoryMap blogId cat)


removeFromCategory :: BlogId -> Category -> Query ()
removeFromCategory blogId (Category cat) =
  delete (DB.BlogCategoryMapKey blogId cat)
