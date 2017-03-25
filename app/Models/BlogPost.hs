{-# LANGUAGE OverloadedStrings, RankNTypes, TypeFamilies #-}

module Models.BlogPost
  ( BlogPost (..)
  , insertBlogPost
  , updateBlogPost
  , getBlogPostId
  , getBlogPostPath
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Time (UTCTime, getCurrentTime)
import Models.BlogIndex (indexToId, blogIndexHandler)
import Models.EventHandlers (executeHandlerQuery)
import Models.Database
import Models.Events
import Routes
import Session
import Text.Markdown (Markdown(..))

data BlogPost =
  BlogPost { content :: Markdown
           , title :: Text
           , publishedTime :: UTCTime
           }


insertBlogPost :: Text -> Text -> UTCTime -> SiteAdminAction BlogId
insertBlogPost title content published = do
  id <- startEvents (map BlogEntry
    [ TitleSet title
    , ContentSet (Markdown $ fromStrict content)
    , PublishedAt published
    ])
  runHandlers
  return id


updateBlogPost :: BlogId -> Text -> Text -> UTCTime -> SiteAdminAction ()
updateBlogPost id title content published = do
  addEvents id (map BlogEntry
    [ TitleSet title
    , ContentSet (Markdown $ fromStrict content)
    , PublishedAt published
    ])
  runHandlers

getBlogPostId :: BlogId -> SiteAction ctx (Maybe BlogPost)
getBlogPostId id = do
  now <- liftIO getCurrentTime
  evs <- getEvents (Just id)
  if null evs
    then return Nothing
    else return . Just $ foldl' update (emptyPost now) evs
  where
    emptyPost = BlogPost (Markdown "") ""
    update post (BlogEntry (TitleSet t)) = post { title = t }
    update post (BlogEntry (ContentSet c)) = post { content = c }
    update post (BlogEntry (PublishedAt t)) = post { publishedTime = t }


getBlogPostPath :: Int -> Int -> Text -> SiteAction ctx (Maybe BlogPost)
getBlogPostPath year month title =
  runSqlAction (indexToId year month title)
  >>= maybe (return Nothing) getBlogPostId


runHandlers :: SiteAdminAction ()
runHandlers =
  runSqlAction $ executeHandlerQuery blogIndexHandler
  
