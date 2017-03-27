{-# LANGUAGE OverloadedStrings,  RankNTypes, GADTs, FlexibleContexts #-}

module Models.Events.Projections where

import Data.Int (Int64)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Models.Database (Query)
import Models.Events.Database (getEvents)
import Models.Events.Types
import Models.Projections
import Text.Markdown(Markdown(..))


getMaybeProjection :: Projection SiteEvent state result -> Int64 -> Query (Maybe result)
getMaybeProjection p id = do
  evs <- map event <$> getEvents (Just id)
  case evs of
    [] -> return Nothing
    _  -> return . Just $ getResult p evs


getProjection :: Projection SiteEvent state result -> Int64 -> Query result
getProjection p id = do
  evs <- map event <$> getEvents (Just id)
  return $ getResult p evs


fromSite :: SiteEvent -> Maybe BlogEntryEvent
fromSite (BlogEntry ev) = Just ev


contentP =
  fromMaybe (Markdown "") <$> lastP contSet
  where
    contSet (ContentSet c) = Just c
    contSet _ = Nothing

titleP =    
  fromMaybe "" <$> lastP titleSet
  where
    titleSet (TitleSet t) = Just t
    titleSet _ = Nothing

publishedAtP now =
  fromMaybe now <$> lastP publAt
  where
    publAt (PublishedAt t) = Just t
    publAt _ = Nothing


categoriesP =
  (\\) <<* addP <<$ remP
  where
    addP = collectP catAdd
    catAdd (AddedToCategory c) = Just c
    catAdd _ = Nothing
    remP = collectP catRem
    catRem (RemovedFromCategory c) = Just c
    catRem _ = Nothing
