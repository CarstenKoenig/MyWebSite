{-# LANGUAGE OverloadedStrings, RankNTypes, TypeFamilies #-}

module Models.BlogPost
  ( BlogPost (..)
  , insertBlogPost
  , updateBlogPost
  , getBlogPostId
  , getBlogPostPath
  , queryBlogPost
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.List (foldl', (\\))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Time (UTCTime, getCurrentTime)
import Models.BlogIndex (indexToId, blogIndexHandler)
import Models.BlogCategory
import qualified Models.BlogCategory as Cat
import Models.Database (Query)
import qualified Models.Database as DB
import Models.Events
import Routes
import Session
import Text.Markdown (Markdown(..))
import Utils.Database


data BlogPost =
  BlogPost { content :: Markdown
           , title :: Text
           , publishedTime :: UTCTime
           , categories :: [Category]
           }


insertBlogPost :: Text -> Text -> UTCTime -> [Category] -> SiteAdminAction BlogId
insertBlogPost title content published categories = runEventAction query
  where
    query = do
      blogId <- newFromEvents (map BlogEntry
                               [ TitleSet title
                               , ContentSet (Markdown $ fromStrict content)
                               , PublishedAt published
                               ])
      let cats = map (BlogEntry . AddedToCategory) categories
      appendEvents blogId cats
      return blogId


updateBlogPost :: BlogId -> Text -> Text -> UTCTime -> [Category] -> SiteAdminAction ()
updateBlogPost id title content published newCategories = do
  oldPost <- getBlogPostId id
  let (added, removed) = diffs (maybe [] categories oldPost) newCategories
  runEventAction $
    appendEvents id (map BlogEntry
                      ([ TitleSet title
                       , ContentSet (Markdown $ fromStrict content)
                       , PublishedAt published
                       ]
                       ++ map AddedToCategory added
                       ++ map RemovedFromCategory removed
                      ))


getBlogPostId :: BlogId -> SiteAction ctx (Maybe BlogPost)
getBlogPostId = runSqlAction . queryBlogPost


queryBlogPost :: BlogId -> Query (Maybe BlogPost)
queryBlogPost id = do
  now <- liftIO getCurrentTime
  evs <- getEvents (Just id)
  if null evs
    then return Nothing
    else return . Just . foldl' update (emptyPost now) $ map event evs
  where
    emptyPost t = BlogPost (Markdown "") "" t []
    update post (BlogEntry (TitleSet t)) = post { title = t }
    update post (BlogEntry (ContentSet c)) = post { content = c }
    update post (BlogEntry (PublishedAt t)) = post { publishedTime = t }
    update post (BlogEntry (AddedToCategory c)) =
      post { categories = c : categories post }
    update post (BlogEntry (RemovedFromCategory c)) =
      post { categories = filter (/= c) $ categories post }


getBlogPostPath :: Int -> Int -> Text -> SiteAction ctx (Maybe BlogPost)
getBlogPostPath year month title =
  runSqlAction (indexToId year month title)
  >>= maybe (return Nothing) getBlogPostId


diffs :: Eq a => [a] -> [a] -> ([a],[a])
diffs xs ys = (added, removed)
  where added = ys \\ xs
        removed = xs \\ ys
