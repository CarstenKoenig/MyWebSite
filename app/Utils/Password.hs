{-# LANGUAGE OverloadedStrings #-}
module Utils.Password
  ( Password (Password)
  , PasswordHash
  , verifyPassword
  , createPasswordHash
  , writePasswordHashToFile
  , readPasswordHashFromFile
  )where


import Data.String (IsString(..))

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Crypto.Hash (Digest, SHA256(..), hash, digestToHexByteString)


newtype Password =
  Password { getPwd :: Text }

instance IsString Password where
  fromString = Password . fromString


newtype PasswordHash =
  PasswordHash { getHash :: ByteString }
  deriving Eq


verifyPassword :: PasswordHash -> Password -> Bool
verifyPassword hash pwd =
  hash == createPasswordHash pwd


createPasswordHash :: Password -> PasswordHash
createPasswordHash =
  PasswordHash .
  digestToHexByteString .
  (hash :: ByteString -> Digest SHA256) .
  encodeUtf8 .
  getPwd


writePasswordHashToFile :: FilePath -> Password -> IO ()
writePasswordHashToFile path =
  B.writeFile path . getHash .  createPasswordHash
  

readPasswordHashFromFile :: FilePath -> IO PasswordHash
readPasswordHashFromFile path =
  PasswordHash <$> B.readFile path
  
