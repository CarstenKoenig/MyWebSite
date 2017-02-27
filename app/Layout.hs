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
      H.link_ [ H.rel_ "shortcut icon", H.href_ "/favicon.ico" ]
      H.meta_ [ H.charset_ "utf-8" ]
      H.meta_ [ H.name_ "description"
              , H.content_ "Blog rund um funktionale Programmierung" ]
      H.meta_ [ H.name_ "author"
              , H.content_ "Carsten König" ]
      H.meta_ [ H.httpEquiv_ "X-UA-Compatible"
              , H.content_ "IE=edge" ]
      H.meta_ [ H.name_ "viewport"
              , H.content_ "width=device-width, initial-scale=1" ]
        
      H.title_ $ toHtml title
      
      -- Bootstrap
      H.link_ [ H.href_ "css/bootstrap.min.css"
              , H.rel_ "stylesheet" ]
      -- Custom css
      H.link_ [ H.href_ "css/site.css"
              , H.rel_ "stylesheet" ]
        
    H.body_ $ do

      H.div_ [ H.class_ "blog-masthead" ] $ do
        BS.container_ $ do
          H.nav_ [ H.class_ "blog-nav" ] $ do
            H.a_ [ H.class_ "blog-nav-item active", H.href_ "/" ] "Blog"
            H.a_ [ H.class_ "blog-nav-item", H.href_ "/aboutMe" ] "über mich"
      
      BS.container_ $ do
        H.div_ [ H.id_ "main", H.role_ "main" ] content

      H.footer_ [ H.class_ "blog-footer" ] $ do
        H.div_ [ H.class_ "col-md-2 text-left" ] $ do
          H.p_ $ do
            H.a_ [ H.href_ "#" ] "nach oben"
        H.div_ [ H.class_ "col-md-8" ] ""

      -- JQuery
      H.script_
        [ H.src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" ]
        T.empty
      -- Bootstrap
      H.script_ [ H.src_ "js/bootstrap.min.js" ] T.empty
      -- MathJax
      H.script_
        [ H.src_ "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML" ]
        T.empty


jumbotron_ :: Html a -> Html a
jumbotron_ = H.div_ [ H.class_ "jumbotron" ]
