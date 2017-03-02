{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds #-}
module Routes where

import Web.Spock
import Web.Spock.Config

import Data.Monoid

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
