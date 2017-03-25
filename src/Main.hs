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
import Models.BlogCategory
import qualified Models.BlogIndex as Index
import Models.BlogPost
import Models.Events (EventHandler, Category(..), forwardEventHandlers)
import Network.HTTP.Types (urlDecode, notFound404)
import Network.Wai (Middleware, Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Routes
import Session
import Utils.Database (initializePool, runOnPool)
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
  cfg <- defaultAppConfig eventHandlers
  pool <- initializePool cfg
  spockCfg <- defaultSpockCfg emptySession (PCPool pool) (SiteState cfg)
  runOnPool pool $ forwardEventHandlers Index.blogIndexHandler
  runSpock 8080 (spock spockCfg app)


eventHandlers :: [EventHandler]
eventHandlers =
  [ Index.blogIndexHandler
  , blogCategoryHandler
  ] 


app :: SiteApp
app = prehook baseHook $ do
  
  adminHash <- adminPwdHash . appConfig  <$> getState
  
  -- serve static files from local static folder
  middleware serveStatic

  timeZone <- liftIO getCurrentTimeZone
  ex <- liftIO example
  
  get root $ renderPage Home $
    Views.BlogPost.page timeZone ex

  get showPostIdR $ \id -> do
    findPost <- getBlogPostId id
    case findPost of
      Just post -> renderPage (ShowId id) $
        Views.BlogPost.page timeZone post
      Nothing ->
        setStatus notFound404

  get showPostPathR $ \year month title -> do
    findPost <- getBlogPostPath year month title
    case findPost of
      Just post -> renderPage (ShowPath year month title) $
        Views.BlogPost.page timeZone post
      Nothing ->
        setStatus notFound404

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
      id <- insertBlogPost title content now []
      redirect (routeLinkText $ ShowId id)
      

    get editPostR $ \id -> do
      findPost <- getBlogPostId id
      renderPage (Edit id) $ Views.EditPost.page (Just id) findPost
    post editPostR $ \id -> do
      title <- fromJust <$> param "title"
      content <- fromJust <$> param "content"
      now <- liftIO getCurrentTime
      updateBlogPost id title content now
      redirect (routeLinkText $ ShowId id)
    


example :: IO BlogPost
example = do
  time <- getCurrentTime
  return $
    BlogPost
    "#Hey you\n`what is up`?\n\n```haskell\nf :: Int -> Bool\nf 0 = True\nf _ = False\n```"
    "Testblogpost" time [Category "Test"]


serveStatic :: Middleware
serveStatic = staticPolicy (addBase "./static")

baseHook :: SiteAction () (HVect '[])
baseHook = return HNil
