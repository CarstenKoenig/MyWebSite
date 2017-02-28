{-# LANGUAGE OverloadedStrings #-}
module Layout
  ( Page(..)
  , LayoutConfig (..)
  , layout
  ) where


import Data.Default
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Lucid (Html, Attribute, toHtml)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import Control.Monad (forM_)


data Page
  = Main
  | AboutMe
  deriving Eq


instance Show Page where
  show Main = "Blog"
  show AboutMe = "Über mich"


data LayoutConfig =
  LayoutConfig { additionalStyles :: Maybe Text
               , activePage :: Page }


instance Default LayoutConfig where
  def = LayoutConfig Nothing Main


layout :: LayoutConfig -> Text -> Html () -> Html ()
layout config title content = do
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

      H.style_ $ fromMaybe "" $ additionalStyles config
        
    H.body_ $ do

      H.div_ [ H.class_ "blog-masthead" ] $ do
        BS.container_ $ nav $ activePage config
      
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


href :: Page -> Attribute
href Main = H.href_ "/"
href AboutMe = H.href_ "/aboutMe"


nav :: Page -> Html ()
nav active = do
  H.nav_ [ H.class_ "blog-nav" ] $ do
     forM_ [ Main, AboutMe ] $ navItem active


navItem :: Page -> Page -> Html ()
navItem active item
  | active == item =
    H.a_ [ H.class_ "blog-nav-item active", href item] (toHtml $ show item)
  | otherwise =
    H.a_ [ H.class_ "blog-nav-item", href item ] (toHtml $ show item)
    

jumbotron_ :: Html a -> Html a
jumbotron_ = H.div_ [ H.class_ "jumbotron" ]
