{-# LANGUAGE OverloadedStrings, RankNTypes, TypeFamilies #-}

module Models.BlogPost
  ( BlogPost (..)
  , insertBlogPost
  , updateBlogPost
  , getBlogPostId
  , getBlogPostPath
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.List (foldl', (\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Time (UTCTime, getCurrentTime)
import Models.BlogCategory
import qualified Models.BlogCategory as Cat
import Models.BlogIndex (indexToId, blogIndexHandler)
import Models.Database (Query)
import qualified Models.Database as DB
import Models.Events
import Models.Projections
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
getBlogPostId id = do
  now <- liftIO getCurrentTime
  runSqlAction $ getMaybeProjection (liftP fromSite $ blogPostP now) id


blogPostP now =
  BlogPost <<* contentP <<$ titleP <<$ publishedAtP now <<$ categoriesP


getBlogPostPath :: Int -> Int -> Text -> SiteAction ctx (Maybe BlogPost)
getBlogPostPath year month title =
  runSqlAction (indexToId year month title)
  >>= maybe (return Nothing) getBlogPostId


diffs :: Eq a => [a] -> [a] -> ([a],[a])
diffs xs ys = (added, removed)
  where added = ys \\ xs
        removed = xs \\ ys
