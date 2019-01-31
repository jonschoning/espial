
module ModelCustom where

import Prelude

import Crypto.BCrypt as Import hiding (hashPassword)
import Database.Persist.Sql
import Safe (fromJustNote)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import System.Entropy (getEntropy)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS

mkSlug :: Int -> IO T.Text
mkSlug size =
  TE.decodeUtf8 . LBS.toStrict . BB.toLazyByteString . BB.byteStringHex <$>
  getEntropy size

-- * Bookmark Slug

newtype BmSlug = BmSlug
  { unBmSlug :: T.Text
  } deriving (Eq, PersistField, PersistFieldSql, Show, Read, Ord, A.FromJSON, A.ToJSON)

mkBmSlug :: IO BmSlug
mkBmSlug = BmSlug <$> mkSlug 6

-- * Note Slug

newtype NtSlug = NtSlug
  { unNtSlug :: T.Text
  } deriving (Eq, PersistField, PersistFieldSql, Show, Read, Ord, A.FromJSON, A.ToJSON)

mkNtSlug :: IO NtSlug
mkNtSlug = NtSlug <$> mkSlug 10

-- * Model Crypto

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
  mPassword <- hashPasswordUsingPolicy policy (TE.encodeUtf8 rawPassword)
  return
    (BCrypt (TE.decodeUtf8 (fromJustNote "Invalid hashing policy" mPassword)))

validatePasswordHash :: BCrypt -> T.Text -> Bool
validatePasswordHash hash' pass = do
  validatePassword (TE.encodeUtf8 (unBCrypt hash')) (TE.encodeUtf8 pass)
