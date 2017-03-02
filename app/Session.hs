{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes #-}
module Session where

import Web.Spock

import Control.Monad.Trans
import Data.Monoid
import Data.IORef

import Utils.Password (PasswordHash, Password(..))
import qualified Utils.Password as Pwd

import Routes

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
adminLogon hash pwd =
  if Pwd.verifyPassword hash pwd then do
    modifySession $  \ session -> session { logon = Admin }
    redirect $ renderRoute adminR
  else do
    modifySession $  \ session -> session { logon = Guest }
    redirect $ renderRoute loginR


logout ::  SiteAction ctx ()
logout = do
    modifySession $  \ session -> session { logon = Guest }
    redirect "/"


requireAdmin :: SiteAction ctx a -> SiteAction ctx a
requireAdmin action =
   do sess <- readSession
      case logon sess of
         Guest -> redirect $ renderRoute loginR
         Admin -> action


