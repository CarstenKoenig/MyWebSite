{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes #-}
module Config
 (AppConfig(..)
 , defaultAppConfig
 ) where

import Data.Text (Text)
import Database.Persist.Postgresql(ConnectionString)
import Models.Events
import Utils.Password (PasswordHash, readPasswordHashFromFile)
import Web.Spock

data AppConfig
  = AppConfig
    { appConfigDb :: ConnectionString
    , adminPwdHash :: PasswordHash
    , handlers :: [EventHandler]
    }


defaultAppConfig :: [EventHandler] -> IO AppConfig
defaultAppConfig handlers = do
  let dbConnStr = "host=localhost port=5432 user=blog dbname=webapp password=blog"
  pwdHash <- readPasswordHashFromFile "admin.pwd"
  return $ AppConfig dbConnStr pwdHash handlers
