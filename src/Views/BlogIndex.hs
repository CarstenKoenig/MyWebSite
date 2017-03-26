{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Views.BlogIndex
  ( monthPage
  ) where

import Data.Monoid((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (TimeZone, TimeOfDay(..), utcToLocalTime, localTimeOfDay, toGregorian, localDay)
import Layout (Page (Page))
import Lucid (Html, toHtml, toHtmlRaw)
import qualified Lucid.Bootstrap as BS
import qualified Lucid.Html5 as H
import Models.BlogIndex
import Models.Events
import Routes
import Text.Printf
import Web.Spock


monthPage :: TimeZone -> Int -> Int -> [BlogIndex] -> Page
monthPage tz year month items =
  Page Nothing title $ pageContent tz year month items
  where
    title = pageTitle year month


pageContent :: TimeZone -> Int -> Int -> [BlogIndex] -> Html ()
pageContent tz year month items =
  H.div_ [ H.class_ "col-sm-8" ] $ do
    H.h3_ [] (toHtml $ pageTitle year month)
    H.div_ [ H.class_ "list-group" ] $ mapM_ (itemContent tz year month) items


itemContent :: TimeZone -> Int -> Int -> BlogIndex -> Html ()
itemContent tz year month ind =
  H.a_ [ H.class_ "list-group-item", H.href_ link ] $
    H.span_ [] $ do
      H.strong_ (toHtml (blogIndexCaption ind))
      " "
      toHtml (metaInfo tz ind)      
  where link = routeLinkText $ ShowPath year month (blogIndexTitle ind)


pageTitle :: Int -> Int -> Text
pageTitle year month =
  "Beiträge in " <> T.pack (show month) <> "." <> T.pack (show year)


metaInfo :: TimeZone -> BlogIndex -> Text
metaInfo zone ind = 
  T.pack $ printf "veröffentlicht am %02d. um %02d:%02d" d h m
  where publishedAt        = utcToLocalTime zone $ blogIndexPublished ind
        (TimeOfDay h m _)  = localTimeOfDay publishedAt
        (_, _, d)         = toGregorian $ localDay publishedAt
