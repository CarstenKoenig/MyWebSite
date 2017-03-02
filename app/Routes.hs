{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds #-}
module Routes where

import Web.Spock
import Web.Spock.Config

import Network.Wai (pathInfo)

import Control.Monad.IO.Class (MonadIO)

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Web.Routing.Combinators (PathState(..))


-- * ROUTES

aboutMeR :: Path '[] Open
aboutMeR = "aboutMe"

logoutR :: Path '[] Open
logoutR = "logout"

loginR :: Path '[] Open
loginR = "login"

adminR :: Path '[] Open
adminR = "admin"


data Route
  = Home
  | AboutMe
  | Login
  deriving Eq


instance Show Route where
  show Home = "Home"
  show AboutMe = "Über mich"
  show Login = "Logon"


routeLinkText :: Route -> Text
routeLinkText Home = renderRoute "/"
routeLinkText AboutMe = renderRoute aboutMeR
routeLinkText Login = renderRoute loginR


requestRoute :: MonadIO m => ActionCtxT ctx m Route
requestRoute = do
  req <- request
  case pathInfo req of
    [] -> return Home
    part:_
      | part == T.tail (renderRoute aboutMeR) -> return AboutMe
      | part == T.tail (renderRoute loginR) -> return Login
