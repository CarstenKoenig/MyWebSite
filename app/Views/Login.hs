{-# LANGUAGE OverloadedStrings #-}

module Views.Login
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


page :: Page
page = Page Nothing "Login" pageContent


pageContent :: Html ()
pageContent = do
  H.div_ [ H.class_ "col-sm-8" ] $ do
    H.h3_ "Admin-Passwort wird benötigt..."
    H.form_ [ H.class_ "form-inline"
            , H.action_ $ renderRoute loginR
            , H.method_ "post"  ] $ do
      H.div_ [ H.class_ "form-group" ] $ do
        H.label_ [ H.class_ "sr-only"
                 , H.for_ "inputPassword" ] "Password"
        H.input_ [ H.type_ "password"
                 , H.class_ "form-control"
                 , H.id_ "inputPassword"
                 , H.name_ "pwd"
                 , H.placeholder_ "Passwort" ]
      H.button_ [ H.type_ "submit"
                , H.class_ "btn btn-default" ] "Ok"
