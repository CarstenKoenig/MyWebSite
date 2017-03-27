{-# LANGUAGE OverloadedStrings #-}
module Views.Error
  ( page
  ) where

import Web.Spock
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import Lucid (Html, toHtml)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS
import Layout (Page (Page))
import Routes
import Network.HTTP.Types.Status (Status(..))


page :: Status -> Page
page status = Page Nothing "Something went wrong..." $ content status


content :: Status -> Html ()
content status =
  H.div_ [ H.id_ "error"] $
    BS.row_ $ do
      col_ 2 ""
      col_ 8 $ do
        H.h2_ [] "Something went wrong..."
        H.span_ [] (toHtml $ statusMessage status)
      col_ 2 ""
    

col_ :: Int -> Html a -> Html a
col_ n = H.div_ [ H.class_ . T.pack $ "col-md-" ++ show n ]
