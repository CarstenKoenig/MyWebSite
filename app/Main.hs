{-# LANGUAGE OverloadedStrings, RankNTypes, DataKinds #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Network.Wai (Middleware, Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

import Control.Monad
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.IORef

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time ( getCurrentTime )
import Data.Time.LocalTime (getCurrentTimeZone)

import Network.HTTP.Types (urlDecode)
import Web.Routing.Combinators (PathState(..))

import Lucid (Html, renderText)
import qualified Lucid.Html5 as H

import Layout (layout)
import Views.AboutMe
import Views.BlogPost
import Views.Login

import Utils.Password (Password(..), PasswordHash)
import qualified Utils.Password as Pwd

import Session
import Routes


main :: IO ()
main = do
  ref <- newIORef 0
  adminHash <- liftIO $ Pwd.readPasswordHashFromFile "admin.pwd"
  spockCfg <- defaultSpockCfg emptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg $ app adminHash)


app :: PasswordHash -> SpockM () SiteSession SiteState ()
app adminHash = do
  -- serve static files from local static folder
  middleware serveStatic

  timeZone <- liftIO getCurrentTimeZone
  ex <- liftIO example
  
  get root $ renderHtml $ Views.BlogPost.page timeZone ex

  get aboutMeR $ renderHtml Views.AboutMe.page

  getpost logoutR logout
    
  get loginR $ renderHtml Views.Login.page
  post loginR $ do
    pwd <- fromMaybe "" <$> param "pwd"
    adminLogon adminHash $ Password pwd
    
  
  get adminR $ requireAdmin $ text "hi Admin"

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


renderHtml :: MonadIO m => ActionCtxT ctx m (Html a) -> ActionCtxT ctx m a
renderHtml = fmap (TL.toStrict . renderText) >=> html
