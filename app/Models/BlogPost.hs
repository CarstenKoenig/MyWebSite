{-# LANGUAGE OverloadedStrings #-}

module Models.BlogPost
  ( BlogPost (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Text.Markdown (Markdown)


data BlogPost =
  BlogPost { content :: Markdown
           , title :: Text
           , publishedTime :: UTCTime
           }
