{-# LANGUAGE OverloadedStrings, RankNTypes, DataKinds #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Network.Wai (Middleware, Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, addBase)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logger (runNoLoggingT)

import Data.HVect (HVect(..))
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

import Database.Persist.Postgresql (SqlBackend, createPostgresqlPool)

import Lucid (Html)
import qualified Lucid.Html5 as H

import Layout (Page, renderPage)
import Views.AboutMe
import Views.BlogPost
import Views.Login

import qualified Models.Database as DB

import Utils.Password (Password(..), PasswordHash)
import qualified Utils.Password as Pwd

import Config
import Session
import Routes


main :: IO ()
main = do
  cfg <- defaultAppConfig
  pool <- DB.initializePool cfg
  spockCfg <- defaultSpockCfg emptySession (PCPool pool) (SiteState cfg)
  runSpock 8080 (spock spockCfg app)


app :: SiteApp
app = prehook baseHook $ do
  
  adminHash <- adminPwdHash . appConfig  <$> getState
  
  -- serve static files from local static folder
  middleware serveStatic

  timeZone <- liftIO getCurrentTimeZone
  ex <- liftIO example
  
  get root $ renderPage Home $
    Views.BlogPost.page timeZone ex

  get aboutMeR $ renderPage AboutMe $
    Views.AboutMe.page

  getpost logoutR logout
    
  get loginR $ renderPage Login $
    Views.Login.page
  post loginR $ do
    pwd <- fromMaybe "" <$> param "pwd"
    adminLogon adminHash $ Password pwd
    
  
  prehook adminHook $
    get adminR $ text "hi Admin"


example :: IO BlogPost
example = do
  time <- getCurrentTime
  return $
    BlogPost
    "#Hey you\n`what is up`?\n\n```haskell\nf :: Int -> Bool\nf 0 = True\nf _ = False\n```"
    "Testblogpost" time


serveStatic :: Middleware
serveStatic = staticPolicy (addBase "./static")

baseHook :: SiteAction () (HVect '[])
baseHook = return HNil
