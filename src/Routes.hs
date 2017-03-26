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

showPostIdR :: Path '[BlogId] Open
showPostIdR = var

showPostPathR :: Path '[Int,Int,Text] Open
showPostPathR = var <//> var <//> var

showMonthPathR :: Path '[Int,Int] Open
showMonthPathR = var <//> var

editPostR :: Path '[BlogId] Open
editPostR = "edit" <//> var

newPostR :: Path '[] Open
newPostR = "new"


data Route
  = Home
  | AboutMe
  | Login
  | ShowId BlogId
  | ShowPath Int Int Text
  | ShowMonth Int Int
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
  show (ShowId _) = "Eintrag ansehen"
  show (ShowMonth _ _) = "Monat ansehen"
  show ShowPath {} = "Eintrag ansehen"


routeLinkText :: Route -> Text
routeLinkText Home = renderRoute "/"
routeLinkText AboutMe = renderRoute aboutMeR
routeLinkText Login = renderRoute loginR
routeLinkText (ShowId id) = renderRoute showPostIdR id
routeLinkText (ShowPath y m t) = renderRoute showPostPathR y m t
routeLinkText (ShowMonth y m) = renderRoute showMonthPathR y m
routeLinkText New = renderRoute newPostR
routeLinkText (Edit id) = renderRoute editPostR id

