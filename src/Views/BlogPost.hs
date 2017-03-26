{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Views.BlogPost
  ( BlogPost (..)
  , page
  ) where

import Web.Spock
import Control.Monad.IO.Class (MonadIO)

import Control.Monad (forM_)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime, toGregorian)
import Data.Time.LocalTime (TimeZone, TimeOfDay (..)
                           , utcToLocalTime, localTimeOfDay, localDay)
import Models.Events
import Text.Blaze (preEscapedText, preEscapedToMarkup)
import qualified Text.Blaze.Html as TBH
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (Markdown(..), MarkdownSettings(..), markdown)
import Text.Printf


import Lucid (Html, toHtml, toHtmlRaw)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import Skylighting ( TokenizerConfig(..), SourceLine)
import qualified Skylighting as Sky       


import Layout (Page (Page))
import Models.BlogPost (BlogPost (..))


page :: TimeZone -> BlogPost -> Page
page timezone post =
  Page (Just cssStyles) (title post) $ pageContent timezone post


pageContent :: TimeZone -> BlogPost -> Html ()
pageContent timeZone post =
  H.div_ [ H.class_ "col-sm-8 blog-main" ] $ do
    H.p_ [ H.class_ "blog-post-meta" ] $ categoryLabels post
    H.h2_ [ H.class_ "blog-post-title" ] $ toHtml . title $ post
    H.p_ [ H.class_ "blog-post-meta" ] $ toHtml $ metaInfo timeZone post
    toHtmlRaw $ renderBlogPost post


metaInfo :: TimeZone -> BlogPost -> Text
metaInfo zone blogPost = 
  T.pack $ printf "veröffentlicht am %02d.%02d.%d um %02d:%02d" d mn y h m
  where publishedAt        = utcToLocalTime zone $ publishedTime blogPost
        (TimeOfDay h m _)  = localTimeOfDay publishedAt
        (y, mn, d)         = toGregorian $ localDay publishedAt


categoryLabels :: BlogPost -> Html ()
categoryLabels blogPost = forM_ (categories blogPost) label
  where
    label :: Category -> Html ()
    label (Category name) =
      H.span_ [ H.class_ "label label-info" ] (toHtml name)


renderBlogPost :: BlogPost -> Text
renderBlogPost blogPost =
  TL.toStrict . renderHtml $ markdown def
     { msBlockCodeRenderer = renderer
     , msXssProtect        = False
     } md
  where
    (Markdown md) = content blogPost
    renderer (Just "math") (src,_) = renderMath src
    renderer (Just "iframe") (src,_) = renderFrame src
    renderer lang (src,_) = renderLang lang src


renderLang :: Maybe Text -> Text -> TBH.Html
renderLang lang src =
  Sky.formatHtmlBlock Sky.defaultFormatOpts
  $ highlightAs (fromMaybe "haskell" lang) src


renderFrame :: Text -> TBH.Html
renderFrame src =
  preEscapedText $
  T.concat [ "<iframe width=\"560\" height=\"315\" "
           , "src=\""
           , src
           , "\" frameborder=\"0\" allowfullscreen></iframe>" ]


renderMath :: Text -> TBH.Html
renderMath src =
  preEscapedToMarkup $
  T.concat ["$$\n", src, "\n$$"]


cssStyles :: Text
cssStyles = T.pack $ Sky.styleToCss Sky.pygments


highlightAs :: Text -> Text -> [SourceLine]
highlightAs lang source =
  case Sky.syntaxByName Sky.defaultSyntaxMap lang of
    Nothing -> []
    Just syntax ->
      case Sky.tokenize config syntax source of
        Left _ -> []
        Right lines -> lines
  where
    config = TokenizerConfig Sky.defaultSyntaxMap False
  
