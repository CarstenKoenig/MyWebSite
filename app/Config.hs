{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes #-}
module Config
 (AppConfig(..)
 , defaultAppConfig
 ) where

import Web.Spock

import Database.Persist.Postgresql(ConnectionString)

import Utils.Password (PasswordHash, readPasswordHashFromFile)

import Data.Text (Text)

data AppConfig
  = AppConfig
    { appConfigDb :: ConnectionString
    , adminPwdHash :: PasswordHash
    }


defaultAppConfig :: IO AppConfig
defaultAppConfig = do
  let dbConnStr = "host=localhost port=5432 user=blog dbname=webapp password=blog"
  pwdHash <- readPasswordHashFromFile "admin.pwd"
  return $ AppConfig dbConnStr pwdHash
