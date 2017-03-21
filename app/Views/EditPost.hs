{-# LANGUAGE OverloadedStrings #-}

module Views.EditPost
  ( page
  ) where

import Web.Spock
import Control.Monad.IO.Class (MonadIO)

import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Lucid (Html, toHtml, toHtmlRaw)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import Layout (Page(Page))
import Routes


page :: BlogId -> Page
page = Page Nothing "Edit Post" . pageContent


pageContent :: BlogId -> Html ()
pageContent id = do
  H.div_ [ H.class_ "col-sm-8" ] $ do
    H.form_ [ H.class_ "form-horizontal"
            , H.action_ $ renderRoute editPostR id
            , H.method_ "post"  ] $ do
      H.div_ [ H.class_ "form-group" ] $ do
        H.label_ [ H.for_ "title" ] "Titel"
        H.input_ [ H.type_ "text"
                 , H.class_ "form-control"
                 , H.id_ "title"
                 , H.name_ "title"
                 , H.placeholder_ "Titel des Eintrags" ]
      H.div_ [ H.class_ "form-group" ] $ do
        H.textarea_
          [ H.class_ "form-control"
          , H.rows_ "35"
          , H.cols_ "80"
          , H.name_ "content"
          , H.placeholder_ "Eintragstext..." ] ""
      H.button_ [ H.type_ "submit"
                , H.class_ "btn btn-default" ] "Speichern"
