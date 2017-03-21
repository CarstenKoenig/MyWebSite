{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes, DataKinds, TypeOperators, FlexibleContexts #-}
module Session where

import Web.Spock

import Control.Monad.Trans

import Data.HVect (HVect(..), ListContains(..))
import Data.Monoid
import Data.IORef

import Utils.Password (PasswordHash, Password(..))
import qualified Utils.Password as Pwd

import Database.Persist.Postgresql (SqlBackend)

import Routes
import Config

type SiteApp = SpockM SqlBackend SiteSession SiteState ()
type SiteAction ctx a = SpockActionCtx ctx SqlBackend SiteSession SiteState a
type SiteAdminAction a
  = forall n xs . ListContains n IsAdmin xs => SiteAction (HVect xs) a

newtype SiteSession =
  SiteSession { logon :: LogonStatus }

data SiteState =
  SiteState { appConfig :: AppConfig }


data LogonStatus
  = Guest
  | Admin
  deriving (Eq, Show)


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


data IsAdmin = IsAdmin

adminHook :: m ~ WebStateM con SiteSession st =>
             ActionCtxT (HVect xs) m (HVect (IsAdmin ': xs))
adminHook = do
  oldCtx <- getContext
  sess <- readSession
  case logon sess of
    Guest -> redirect $ renderRoute loginR
    Admin -> return (IsAdmin :&: oldCtx)

