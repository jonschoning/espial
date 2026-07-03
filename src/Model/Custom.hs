module Model.Custom where

import Crypto.BCrypt (HashingPolicy (..), hashPasswordUsingPolicy, validatePassword)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as A
import Data.Base64.Types qualified as Base64
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.Persist.Sql
import System.Entropy (getEntropy)
import Prelude

mkSlug :: Int -> IO T.Text
mkSlug size =
  TE.decodeUtf8 . LBS.toStrict . BB.toLazyByteString . BB.byteStringHex
    <$> getEntropy size

-- * Bookmark Slug

newtype BmSlug = BmSlug
  { unBmSlug :: T.Text
  }
  deriving (Eq, PersistField, PersistFieldSql, Show, Read, Ord, A.FromJSON, A.ToJSON)

mkBmSlug :: IO BmSlug
mkBmSlug = BmSlug <$> mkSlug 6

-- * Note Slug

newtype NtSlug = NtSlug
  { unNtSlug :: T.Text
  }
  deriving (Eq, PersistField, PersistFieldSql, Show, Read, Ord, A.FromJSON, A.ToJSON)

mkNtSlug :: IO NtSlug
mkNtSlug = NtSlug <$> mkSlug 10

-- * Password Hashing

newtype PasswordHash = PasswordHash
  { unPasswordHash :: T.Text
  }
  deriving (Eq, PersistField, PersistFieldSql, Show, Ord, A.FromJSON, A.ToJSON)

bcryptPolicy :: HashingPolicy
bcryptPolicy = HashingPolicy {preferredHashCost = 12, preferredHashAlgorithm = "$2a$"}

data HashAlgoConfig
  = HashAlgoBCrypt HashingPolicy

-- | Hash a password with BCrypt.
hashPasswordBCrypt :: T.Text -> IO PasswordHash
hashPasswordBCrypt = hashPasswordWith (HashAlgoBCrypt bcryptPolicy)

hashPasswordBCryptWithPolicy :: HashingPolicy -> T.Text -> IO PasswordHash
hashPasswordBCryptWithPolicy policy = hashPasswordWith (HashAlgoBCrypt policy)

-- | True when the stored hash should be rehashed under 'targetAlgo'
needsRehash :: HashAlgoConfig -> PasswordHash -> Bool
needsRehash (HashAlgoBCrypt _) (PasswordHash stored) = not ("$2" `T.isPrefixOf` stored)

-- | Hash a password under the given algorithm/parameters
hashPasswordWith :: HashAlgoConfig -> T.Text -> IO PasswordHash
hashPasswordWith (HashAlgoBCrypt policy) pwd = do
  mbs <- hashPasswordUsingPolicy policy (TE.encodeUtf8 pwd)
  case mbs of
    Nothing -> ioError (userError "bcrypt: invalid hashing policy")
    Just bs -> pure (PasswordHash (TE.decodeUtf8 bs))

-- | Validate a plaintext password against a stored hash.
validatePasswordHash :: PasswordHash -> T.Text -> Bool
validatePasswordHash ph@(PasswordHash h) pwd
  | "$2" `T.isPrefixOf` h = validateBCrypt ph pwd
  | otherwise = False

validateBCrypt :: PasswordHash -> T.Text -> Bool
validateBCrypt (PasswordHash h) pwd =
  validatePassword (TE.encodeUtf8 h) (TE.encodeUtf8 pwd)

-- * Model Crypto (API key)

newtype ApiKey = ApiKey {unApiKey :: T.Text}

newtype HashedApiKey
  = HashedApiKey T.Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (PersistField, PersistFieldSql, A.FromJSON, A.ToJSON)

generateApiKey :: IO ApiKey
generateApiKey = do
  bytes <- getEntropy 32
  pure $ ApiKey $ Base64.extractBase64 $ Base64Url.encodeBase64 bytes

hashApiKey :: ApiKey -> HashedApiKey
hashApiKey =
  HashedApiKey
    . TE.decodeUtf8
    . Base64.extractBase64
    . Base64Url.encodeBase64'
    . sha256Bytes
    . TE.encodeUtf8
    . unApiKey
  where
    sha256Bytes :: ByteString -> ByteString
    sha256Bytes = SHA256.hash
