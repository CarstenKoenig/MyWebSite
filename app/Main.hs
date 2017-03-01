{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Network.Wai (Middleware, Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

import Control.Monad.Trans
import Data.Monoid
import Data.IORef

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Time ( getCurrentTime )
import Data.Time.LocalTime (getCurrentTimeZone)

import Lucid (Html, renderText)
import qualified Lucid.Html5 as H

import Layout (layout, Page(..))
import Views.AboutMe
import Views.BlogPost

import qualified Utils.Password as Pwd
import Session

main :: IO ()
main = do
  adminHash <- Pwd.readPasswordHashFromFile "admin.pwd"
  
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg emptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg app)


app :: SpockM () SiteSession SiteState ()
app = do
  -- serve static files from local static folder
  middleware serveStatic

  timeZone <- liftIO getCurrentTimeZone
  ex <- liftIO example
  
  get root $ renderHtml $ Views.BlogPost.page timeZone ex

  get "aboutMe" $ renderHtml Views.AboutMe.page
       
  get "login" $ text "login..."
  get "admin" $ requireAdmin $ text "hi Admin"

  get ("hello" <//> var) $ \name -> do
    DummyAppState ref <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
    text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))


example :: IO BlogPost
example = do
  time <- getCurrentTime
  return $
    BlogPost
    "#Hey you\n`what is up`?\n\n```haskell\nf :: Int -> Bool\nf 0 = True\nf _ = False\n```"
    "Testblogpost" time


serveStatic :: Middleware
serveStatic = staticPolicy (addBase "./static")


renderHtml :: MonadIO m => Html a -> ActionCtxT ctx m a
renderHtml = html . TL.toStrict . renderText
