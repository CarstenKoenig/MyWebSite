{-# LANGUAGE OverloadedStrings #-}
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

import Lucid (Html, renderText)
import qualified Lucid.Html5 as H


data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)


main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)


app :: SpockM () MySession MyAppState ()
app = do
  -- serve static files from local static folder
  middleware serveStatic
  
  get root $ renderHtml helloTemplate
       
  get ("hello" <//> var) $ \name -> do
    DummyAppState ref <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
    text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))


serveStatic :: Middleware
serveStatic = staticPolicy (addBase "./static")


renderHtml :: MonadIO m => Html a -> ActionCtxT ctx m a
renderHtml = html . TL.toStrict . renderText

helloTemplate :: Html ()
helloTemplate = do
  H.h3_ "Hello Lucid"
