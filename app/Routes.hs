{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds #-}
module Routes where

import Data.HVect(HVect(..))
import Data.Int (Int64)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (pathInfo)
import Web.Routing.Combinators (PathState(..))
import Web.Spock
import Web.Spock.Config

newtype RedirectTo = RedirectTo Text
  deriving (Show, Eq)

-- * ROUTES

aboutMeR :: Path '[] Open
aboutMeR = "aboutMe"

logoutR :: Path '[] Open
logoutR = "logout"

loginR :: Path '[] Open
loginR = "login"

adminR :: Path '[] Open
adminR = "admin"

showPostR :: Path '[BlogId] Open
showPostR = var

editPostR :: Path '[BlogId] Open
editPostR = "edit" <//> var

newPostR :: Path '[] Open
newPostR = "new"


data Route
  = Home
  | AboutMe
  | Login
  | Show BlogId
  | Edit BlogId
  | New
  deriving Eq


type BlogId = Int64


instance Show Route where
  show Home = "Home"
  show AboutMe = "Über mich"
  show Login = "Logon"
  show New = "Neuer Eintrag"
  show (Edit _) = "Eintrag editieren"
  show (Show _) = "Eintrag ansehen"


routeLinkText :: Route -> Text
routeLinkText Home = renderRoute "/"
routeLinkText AboutMe = renderRoute aboutMeR
routeLinkText Login = renderRoute loginR
routeLinkText (Show id) = renderRoute showPostR id
routeLinkText New = renderRoute newPostR
routeLinkText (Edit id) = renderRoute editPostR id

