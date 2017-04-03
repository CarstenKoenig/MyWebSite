{-# LANGUAGE OverloadedStrings #-}
module Layout
  ( Page (..)
  , renderPage
  ) where

import Web.Spock

import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Control.Monad
import Control.Monad.IO.Class (MonadIO)

import Lucid (Html, Attribute, toHtml, renderText)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import Control.Monad (forM_)

import Routes


data Page =
  Page { additionalStyles :: Maybe Text
       , title :: Text
       , content :: Html ()
       }


renderPage :: MonadIO m => Route -> Page -> ActionCtxT ctx m a
renderPage route = layout route >=> (html . TL.toStrict . renderText)


layout :: MonadIO m => Route -> Page -> ActionCtxT ctx m (Html ())
layout route page = do
  return $ do
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
        
        H.title_ $ toHtml $ title page
      
        -- Bootstrap
        H.link_ [ H.href_ "/css/bootstrap.min.css"
                , H.rel_ "stylesheet" ]
        -- Custom css
        H.link_ [ H.href_ "/css/site.css"
                , H.rel_ "stylesheet" ]

        H.style_ $ fromMaybe "" $ additionalStyles page
        
      H.body_ $ do

        H.div_ [ H.class_ "blog-masthead" ] $ do
          BS.container_ $ nav route
      
        BS.container_ $ do
          H.div_ [ H.id_ "main", H.role_ "main" ] $ content page

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
        H.script_ [ H.src_ "/js/bootstrap.min.js" ] T.empty
        -- MathJax
        H.script_
          [ H.src_ "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML" ]
          T.empty


href :: Route -> Attribute
href route = H.href_ $ routeLinkText route


nav :: Route -> Html ()
nav active = do
  H.nav_ [ H.class_ "blog-nav" ] $ do
     forM_ [ Home, AboutMe ] $ navItem active


navItem :: Route -> Route -> Html ()
navItem active item
  | active == item =
    H.a_ [ H.class_ "blog-nav-item active", href item] (toHtml $ show item)
  | otherwise =
    H.a_ [ H.class_ "blog-nav-item", href item ] (toHtml $ show item)
    

jumbotron_ :: Html a -> Html a
jumbotron_ = H.div_ [ H.class_ "jumbotron" ]
