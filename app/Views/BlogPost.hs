{-# LANGUAGE OverloadedStrings #-}

module Views.BlogPost
  ( BlogPost (..)
  , page
  ) where

import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, toGregorian)
import Data.Time.LocalTime (TimeZone, TimeOfDay (..), utcToLocalTime, localTimeOfDay, localDay)


import Lucid (Html, toHtml, toHtmlRaw)
import qualified Lucid.Html5 as H
import qualified Lucid.Bootstrap as BS

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Markdown (markdown, Markdown(..), MarkdownSettings(..))
import Skylighting
       ( TokenizerConfig(..), SourceLine
       , formatHtmlBlock, defaultFormatOpts
       , styleToCss, pygments, tokenize, syntaxByName, defaultSyntaxMap)
import Text.Blaze (preEscapedText, preEscapedToMarkup)
import qualified Text.Blaze.Html as TBH
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Printf


import Layout (layout, Page(Main))


data BlogPost =
  BlogPost { content :: Markdown
           , title :: Text
           , publishedTime :: UTCTime
           }


page :: TimeZone -> BlogPost -> Html ()
page timeZone post =
  layout (Just cssStyles) (title post) Main $
  pageContent timeZone post


pageContent :: TimeZone -> BlogPost -> Html ()
pageContent timeZone post = do
  H.div_ [ H.class_ "col-sm-8 blog-main" ] $ do
    H.h2_ [ H.class_ "blog-post-title" ] $ toHtml . title $ post
    H.p_ [ H.class_ "blog-post-meta" ] $ do
      toHtml $ metaInfo timeZone post
    toHtmlRaw $ renderBlogPost post


metaInfo :: TimeZone -> BlogPost -> Text
metaInfo zone blogPost = 
  T.pack $ printf "veröffentlicht am %02d.%02d.%d um %02d:%02d" d mn y h m
  where publishedAt        = utcToLocalTime zone $ publishedTime blogPost
        (TimeOfDay h m _)  = localTimeOfDay publishedAt
        (y, mn, d)         = toGregorian $ localDay publishedAt


renderBlogPost :: BlogPost -> Text
renderBlogPost blogPost =
  TL.toStrict . renderHtml $ markdown def
     { msBlockCodeRenderer = renderer
     , msXssProtect        = False
     } $ md
  where
    (Markdown md) = content blogPost
    renderer (Just "math") (src,_) = renderMath src
    renderer (Just "iframe") (src,_) = renderFrame src
    renderer lang (src,_) = renderLang lang src


renderLang :: Maybe Text -> Text -> TBH.Html
renderLang lang src =
  formatHtmlBlock defaultFormatOpts
  $ highlightAs (fromMaybe "haskell" lang)
  $ src


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
cssStyles = T.pack $ styleToCss pygments


highlightAs :: Text -> Text -> [SourceLine]
highlightAs lang source =
  case syntaxByName defaultSyntaxMap lang of
    Nothing -> []
    Just syntax ->
      case tokenize config syntax source of
        Left _ -> []
        Right lines -> lines
  where
    config = TokenizerConfig defaultSyntaxMap False
  
