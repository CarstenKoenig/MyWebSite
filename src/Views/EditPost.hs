{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Views.EditPost
  ( page
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Layout (Page(Page))
import Lucid (Html, toHtml, toHtmlRaw)
import qualified Lucid.Bootstrap as BS
import qualified Lucid.Html5 as H
import Models.BlogPost (BlogPost(..))
import Models.Events
import Routes
import Text.Markdown(Markdown(..))
import Web.Spock


page :: Maybe BlogId -> Maybe BlogPost -> Page
page Nothing   _    = Page Nothing "New Post"  pageContentNew
page (Just id) post = Page Nothing "Edit Post" $ pageContentEdit id post


pageContentNew :: Html ()
pageContentNew = pageContent Nothing (renderRoute newPostR)


pageContentEdit :: BlogId -> Maybe BlogPost -> Html ()
pageContentEdit id post = pageContent post (renderRoute editPostR id)


pageContent :: Maybe BlogPost -> Text -> Html ()
pageContent post link =
  H.div_ [ H.class_ "col-sm-8" ] $
    H.form_ [ H.class_ "form-horizontal"
            , H.action_ link
            , H.method_ "post"  ] $ do
      H.div_ [ H.class_ "form-group" ] $ do
        H.label_ [ H.for_ "title" ] "Titel"
        H.input_ [ H.type_ "text"
                 , H.class_ "form-control"
                 , H.id_ "title"
                 , H.name_ "title"
                 , H.placeholder_ "Titel des Eintrags"
                 , H.value_ blogTitle ]
      H.div_ [ H.class_ "form-group" ] $
        H.textarea_
          [ H.class_ "form-control"
          , H.rows_ "35"
          , H.cols_ "80"
          , H.name_ "content"
          , H.placeholder_ "Eintragstext..."
          ] blogContent
      H.div_ [ H.class_ "form-group" ] $ do
        H.label_ [ H.for_ "cats" ] "Kategorien"
        H.input_ [ H.type_ "text"
                 , H.class_ "form-control"
                 , H.id_ "cats"
                 , H.name_ "categories"
                 , H.placeholder_ "Kategorien durch , oder ; getrennt"
                 , H.value_ blogCats ]
      H.button_ [ H.type_ "submit"
                , H.class_ "btn btn-default" ] "Speichern"
  where
    blogTitle = maybe "" title post
    blogContent = maybe "" (mdText . content) post
    blogCats = T.intercalate ", " . map categoryName $ maybe [] categories post
    mdText (Markdown text) = toHtmlRaw $ TL.toStrict text
