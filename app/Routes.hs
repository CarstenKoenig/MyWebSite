{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds #-}
module Routes where

import Web.Spock
import Web.Spock.Config

import Network.Wai (pathInfo)

import Data.HVect(HVect(..))
import Data.Int (Int64)
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

editPostR :: Path '[BlogId] Open
editPostR = "edit" <//> var


data Route
  = Home
  | AboutMe
  | Login
  | Edit BlogId
  deriving Eq


type BlogId = Int64


instance Show Route where
  show Home = "Home"
  show AboutMe = "Über mich"
  show Login = "Logon"
  show (Edit _) = "Eintrag editieren"


routeLinkText :: Route -> Text
routeLinkText Home = renderRoute "/"
routeLinkText AboutMe = renderRoute aboutMeR
routeLinkText Login = renderRoute loginR
routeLinkText (Edit id) = renderRoute editPostR id

