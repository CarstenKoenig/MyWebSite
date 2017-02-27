{-# LANGUAGE OverloadedStrings #-}
module Layout
  ( layout
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Lucid (Html, toHtml)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS


layout :: Text -> Html () -> Html ()
layout title content = do
  H.doctype_ 
  H.html_ [ H.lang_ "de" ] $ do
    H.head_ $ do
      H.meta_ [ H.charset_ "utf-8" ]
      H.meta_ [ H.httpEquiv_ "X-UA-Compatible"
              , H.content_ "IE=edge" ]
      H.meta_ [ H.name_ "viewport"
              , H.content_ "width=device-width, initial-scale=1" ]
      H.title_ "Funktionle Programmierung und mehr..."
      -- Bootstrap
      H.link_ [ H.href_ "css/bootstrap.min.css"
              , H.rel_ "stylesheet" ]
        
    H.body_ $ do
      BS.container_ $ do
        BS.row_ $ do
          jumbotron_ $
            H.h1_ $ toHtml title

        content
      -- JQuery
      H.script_
        [ H.src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" ]
        T.empty
      -- Bootstrap
      H.script_ [ H.src_ "js/bootstrap.min.js" ] T.empty


jumbotron_ :: Html a -> Html a
jumbotron_ = H.div_ [ H.class_ "jumbotron" ]
