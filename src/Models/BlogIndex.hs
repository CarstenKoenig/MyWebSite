{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}


module Models.BlogIndex
  ( blogIndexHandler
  , indexToId
  , monthView
  )
where

import Control.Monad.IO.Class(liftIO)
import qualified Data.Char as C
import Data.Int (Int64)
import Data.List (nub)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import Database.Persist
import Database.Persist.Postgresql
import Models.BlogPost (BlogPost(..))
import Models.Database (Query)
import qualified Models.Database as DB
import Models.Events
import Routes


data BlogIndex =
  BlogIndex { blogIndexYear :: Int
            , blogIndexMonth :: Int
            , blogIndexTitle :: Text
            , blogIndexPost :: BlogPost
            } deriving (Show, Eq)


blogIndexHandler :: EventHandler
blogIndexHandler = EventHandler name handle
  where
    name = "BlogIndex"
    handle ev =
      query (aggregateId ev) (event ev)
    query aggId (BlogEntry (TitleSet title)) =
      setBlogIndexTitle aggId (validateTitle title)
    query aggId (BlogEntry (PublishedAt time)) =
      setBlogIndexDate aggId time
    query _ _ =
      return ()


validateTitle :: Text -> Text
validateTitle = T.toLower . T.filter validLetter . T.map replace
  where
    validLetter '_' = True
    validLetter '-' = True
    validLetter c = C.isAscii c && not (C.isPunctuation c)
    replace c
      | C.isSpace c = '-'
      | otherwise = c


----------------------------------------------------------------------
-- DB Queries

indexToId :: Int -> Int -> Text -> Query (Maybe BlogId)
indexToId year month title =
  fmap (DB.blogIndexAggregateId . entityVal) <$> getBy (DB.UniquePath year month title)


monthView :: Int -> Int -> Query [(Text, BlogId)]
monthView year month =
  fmap (view . entityVal) <$>
  selectList [DB.BlogIndexYear ==. year, DB.BlogIndexMonth ==. month ]
             [Desc DB.BlogIndexAggregateId]
  where
    view row =
      (DB.blogIndexTitle row, DB.blogIndexAggregateId row)


yearView :: Int -> Query [Int]
yearView year =
  nub . fmap (view . entityVal) <$>
  selectList [DB.BlogIndexYear ==. year ]
             [Desc DB.BlogIndexMonth]
  where
    view = DB.blogIndexMonth
      

setBlogIndexTitle :: BlogId -> Text -> Query ()
setBlogIndexTitle blogId title = do
  now <- liftIO getCurrentTime
  let (year, month, _) = toGregorian (utctDay now)
  found <- get (DB.BlogIndexKey blogId)
  case found of
    Nothing ->
      insertKey (DB.BlogIndexKey blogId)
                (DB.BlogIndex (fromIntegral year) month title blogId)
    Just _ ->
      update (DB.BlogIndexKey blogId)
             [ DB.BlogIndexTitle =. title ]


setBlogIndexDate :: BlogId -> UTCTime -> Query ()
setBlogIndexDate blogId time = do
  let (year, month, _) = toGregorian (utctDay time)
      title = T.pack $ show blogId
  found <- get (DB.BlogIndexKey blogId)
  case found of
    Nothing ->
      insertKey (DB.BlogIndexKey blogId)
                (DB.BlogIndex (fromIntegral year) month title blogId) 
    Just _ ->
      update (DB.BlogIndexKey blogId)
             [ DB.BlogIndexYear =. fromIntegral year
             , DB.BlogIndexMonth =. month ]  
