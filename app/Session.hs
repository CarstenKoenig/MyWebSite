{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes, DataKinds, TypeOperators, FlexibleContexts #-}
module Session where

import Web.Spock

import Control.Monad.Trans

import Config
import Data.HVect (HVect(..), ListContains(..))
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import Database.Persist.Postgresql (SqlBackend)
import Network.Wai(rawPathInfo)
import Routes
import Utils.Password (PasswordHash, Password(..))
import qualified Utils.Password as Pwd

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


adminLogon :: Maybe RedirectTo -> PasswordHash -> Password -> SiteAction ctx ()
adminLogon redirectTo hash pwd =
  if Pwd.verifyPassword hash pwd then do
    modifySession $  \ session -> session { logon = Admin }
    redirect $ redTo
  else do
    modifySession $  \ session -> session { logon = Guest }
    redirect $ renderRoute loginR
  where
    redTo =
      case redirectTo of
        (Just (RedirectTo url)) -> url
        Nothing -> "/"


logout ::  SiteAction ctx ()
logout = do
    modifySession $  \ session -> session { logon = Guest }
    redirect "/"


data IsAdmin = IsAdmin

adminHook :: m ~ WebStateM con SiteSession st =>
             ActionCtxT (HVect xs) m (HVect (IsAdmin ': xs))
adminHook = do
  url <- decodeUtf8 . rawPathInfo <$> request
  oldCtx <- getContext
  sess <- readSession
  case logon sess of
    Guest -> redirect $ renderRoute loginR `T.append` "?redirect=" `T.append` url
    Admin -> return (IsAdmin :&: oldCtx)

