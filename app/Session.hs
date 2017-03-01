{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Session where

import Web.Spock

import Control.Monad.Trans
import Data.Monoid
import Data.IORef

import Utils.Password (PasswordHash, Password(..))
import qualified Utils.Password as Pwd


type SiteAction ctx a = SpockActionCtx ctx () SiteSession SiteState a

data SiteSession =
  SiteSession { logon :: LogonStatus }


data LogonStatus
  = Guest
  | Admin
  deriving (Eq, Show)


data SiteState = DummyAppState (IORef Int)


emptySession :: SiteSession
emptySession = SiteSession Guest


adminLogon :: PasswordHash -> Password -> SiteAction ctx ()
adminLogon hash pwd = modifySession $
  \ session ->
      if Pwd.verifyPassword hash pwd then
        session { logon = Admin }
      else
        session { logon = Guest }


requireAdmin :: SiteAction ctx a -> SiteAction ctx a
requireAdmin action =
   do sess <- readSession
      case logon sess of
         Guest -> redirect "/login"
         Admin -> action
