
module ModelCrypto where

import Prelude

import Crypto.BCrypt as Import hiding (hashPassword)
import Database.Persist.Sql
import Safe (fromJustNote)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A

policy :: HashingPolicy
policy =
  HashingPolicy
  { preferredHashCost = 12
  , preferredHashAlgorithm = "$2a$"
  }

newtype BCrypt = BCrypt
  { unBCrypt :: T.Text
  } deriving (Eq, PersistField, PersistFieldSql, Show, Ord, A.FromJSON, A.ToJSON)

hashPassword :: T.Text -> IO BCrypt
hashPassword rawPassword = do
  mPassword <- hashPasswordUsingPolicy policy $ TE.encodeUtf8 rawPassword
  return $ BCrypt $ TE.decodeUtf8 $ fromJustNote "Invalid hashing policy" mPassword

passwordMatches :: BCrypt -> T.Text -> Bool
passwordMatches hash' pass =
  validatePassword (TE.encodeUtf8 $ unBCrypt hash') (TE.encodeUtf8 pass)
