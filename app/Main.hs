{-# LANGUAGE OverloadedStrings, RankNTypes, DataKinds #-}
module Main where

import Config
import Control.Monad
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans
import Data.HVect (HVect(..))
import Data.IORef
import Data.Maybe (fromMaybe, fromJust)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Time ( getCurrentTime )
import Data.Time.LocalTime (getCurrentTimeZone)
import Database.Persist.Postgresql (SqlBackend, createPostgresqlPool)
import Layout (Page, renderPage)
import Lucid (Html)
import qualified Lucid.Html5 as H
import Models.BlogPost
import Models.Database (initializePool)
import Models.EventHandlers
import Network.HTTP.Types (urlDecode)
import Network.Wai (Middleware, Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Routes
import Session
import Utils.Password (Password(..), PasswordHash)
import qualified Utils.Password as Pwd
import Views.AboutMe
import Views.BlogPost
import Views.EditPost
import Views.Login
import Web.Routing.Combinators (PathState(..))
import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  cfg <- defaultAppConfig
  pool <- initializePool cfg
  spockCfg <- defaultSpockCfg emptySession (PCPool pool) (SiteState cfg)
  executeHandler pool blogIndexHandler
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

  get showPostR $ \id -> do
    findPost <- getBlogPost id
    case findPost of
      Just post -> renderPage (Show id) $
        Views.BlogPost.page timeZone post
      Nothing ->
        redirect "/"

  get aboutMeR $
    renderPage AboutMe Views.AboutMe.page

  getpost logoutR logout
    
  get loginR $ do
    redTo <- fmap RedirectTo <$> param "redirect"
    renderPage Login $ Views.Login.page redTo
  post loginR $ do
    pwd <- fromMaybe "" <$> param "pwd"
    redTo <- fmap RedirectTo <$> param "redirect"
    adminLogon redTo adminHash $ Password pwd
    
  
  prehook adminHook $ do
    get adminR $ text "hi Admin"

    get newPostR $ renderPage New $
      Views.EditPost.page Nothing Nothing
    post newPostR $ do
      title <- fromJust <$> param "title"
      content <- fromJust <$> param "content"
      now <- liftIO getCurrentTime
      id <- insertBlogPost title content now
      redirect (routeLinkText $ Show id)
      

    get editPostR $ \id -> do
      findPost <- getBlogPost id
      renderPage (Edit id) $ Views.EditPost.page (Just id) findPost
    post editPostR $ \id -> do
      title <- fromJust <$> param "title"
      content <- fromJust <$> param "content"
      now <- liftIO getCurrentTime
      updateBlogPost id title content now
      redirect (routeLinkText $ Show id)
    


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
