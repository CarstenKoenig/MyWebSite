{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Views.BlogIndex
  ( monthPage
  ) where

import Data.Monoid((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Layout (Page (Page))
import Lucid (Html, toHtml, toHtmlRaw)
import qualified Lucid.Bootstrap as BS
import qualified Lucid.Html5 as H
import Models.Events
import Routes
import Web.Spock


monthPage :: Int -> Int -> [(Text, BlogId)] -> Page
monthPage year month items =
  Page Nothing title $ pageContent year month items
  where
    title = pageTitle year month


pageContent :: Int -> Int -> [(Text, BlogId)] -> Html ()
pageContent year month items =
  H.div_ [ H.class_ "col-sm-8" ] $ do
    H.h3_ [] (toHtml $ pageTitle year month)
    H.div_ [ H.class_ "list-group" ] $ mapM_ (itemContent year month) items


itemContent :: Int -> Int -> (Text, BlogId) -> Html ()
itemContent year month (title, blogId) =
  H.a_ [ H.class_ "list-group-item", H.href_ link ] $
    toHtml title
  where link = routeLinkText $ ShowPath year month title


pageTitle :: Int -> Int -> Text
pageTitle year month =
  "Beiträge in " <> T.pack (show month) <> "." <> T.pack (show year)
