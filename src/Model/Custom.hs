module Model.Custom where

import Crypto.BCrypt (HashingPolicy (..), hashPasswordUsingPolicy, validatePassword)
import Crypto.Error (CryptoFailable, maybeCryptoError, throwCryptoErrorIO)
import Crypto.Hash (Digest, SHA256, hash)
import Crypto.KDF.Argon2 qualified as Argon2
import Data.Aeson qualified as A
import Data.Base64.Types qualified as Base64
import Data.ByteArray qualified as BA (constEq, convert)
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.Persist.Sql
import System.Entropy (getEntropy)
import Text.Read (readMaybe)
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

-- | Opaque password hash stored as TEXT in the database.
-- Inspect the prefix to determine the algorithm:
-- @$2a$@ / @$2b$@ indicates BCrypt; @$argon2id$@ indicates Argon2id.
newtype PasswordHash = PasswordHash
  { unPasswordHash :: T.Text
  }
  deriving (Eq, PersistField, PersistFieldSql, Show, Ord, A.FromJSON, A.ToJSON)

bcryptPolicy :: HashingPolicy
bcryptPolicy = HashingPolicy {preferredHashCost = 12, preferredHashAlgorithm = "$2a$"}

-- | Hash a password with BCrypt. Retained for testing BCrypt→Argon2id migration paths.
hashPasswordBCrypt :: T.Text -> IO PasswordHash
hashPasswordBCrypt pwd = do
  mbs <- hashPasswordUsingPolicy bcryptPolicy (TE.encodeUtf8 pwd)
  case mbs of
    Nothing -> ioError (userError "bcrypt: invalid hashing policy")
    Just bs -> pure (PasswordHash (TE.decodeUtf8 bs))

-- | True when the stored hash should be upgraded to Argon2id.
needsRehash :: PasswordHash -> Bool
needsRehash (PasswordHash h) = not ("$argon2id$" `T.isPrefixOf` h)

-- | Argon2id parameters — OWASP recommended minimum: m=65536 KiB, t=3, p=1.
argon2idOptions :: Argon2.Options
argon2idOptions =
  Argon2.Options
    { Argon2.iterations = 3,
      Argon2.memory = 65536,
      Argon2.parallelism = 1,
      Argon2.variant = Argon2.Argon2id,
      Argon2.version = Argon2.Version13
    }

argon2HashLen :: Int
argon2HashLen = 32

-- | Hash a password with Argon2id, producing a PHC-format string.
hashPassword :: T.Text -> IO PasswordHash
hashPassword pwd = do
  salt <- getEntropy 16
  digest <- throwCryptoErrorIO
    (Argon2.hash argon2idOptions (TE.encodeUtf8 pwd) salt argon2HashLen :: CryptoFailable ByteString)
  pure . PasswordHash $
    "$argon2id$v=19$m=65536,t=3,p=1$"
      <> argon2B64Encode salt
      <> "$"
      <> argon2B64Encode digest

-- | Validate a plaintext password against a stored hash.
-- Dispatches to the correct algorithm based on the hash prefix.
validatePasswordHash :: PasswordHash -> T.Text -> Bool
validatePasswordHash ph@(PasswordHash h) pwd
  | "$2" `T.isPrefixOf` h = validateBCrypt ph pwd
  | "$argon2id$" `T.isPrefixOf` h = validateArgon2id ph pwd
  | otherwise = False

validateBCrypt :: PasswordHash -> T.Text -> Bool
validateBCrypt (PasswordHash h) pwd =
  validatePassword (TE.encodeUtf8 h) (TE.encodeUtf8 pwd)

validateArgon2id :: PasswordHash -> T.Text -> Bool
validateArgon2id (PasswordHash stored) pwd = fromMaybe False $ do
  rest <- T.stripPrefix "$argon2id$v=19$m=" stored
  case T.splitOn "$" rest of
    [paramStr, saltB64, hashB64] -> do
      opts <- parseArgon2Params paramStr
      salt <- argon2B64Decode saltB64
      expected <- argon2B64Decode hashB64
      let result = Argon2.hash opts (TE.encodeUtf8 pwd) salt (BS.length expected) :: CryptoFailable ByteString
      computed <- maybeCryptoError result
      pure (BA.constEq computed expected)
    _ -> Nothing

-- | Parse an Argon2 param string of the form @"65536,t=3,p=1"@.
parseArgon2Params :: T.Text -> Maybe Argon2.Options
parseArgon2Params t =
  case T.splitOn "," t of
    [mStr, tPart, pPart] -> do
      m <- readMaybe (T.unpack mStr)
      t' <- readMaybe . T.unpack =<< T.stripPrefix "t=" tPart
      p <- readMaybe . T.unpack =<< T.stripPrefix "p=" pPart
      pure
        Argon2.Options
          { Argon2.iterations = t',
            Argon2.memory = m,
            Argon2.parallelism = p,
            Argon2.variant = Argon2.Argon2id,
            Argon2.version = Argon2.Version13
          }
    _ -> Nothing

-- | Standard base64 without padding, as used in Argon2 PHC strings.
argon2B64Encode :: ByteString -> T.Text
argon2B64Encode bs = T.dropWhileEnd (== '=') . TE.decodeUtf8 $ (BAE.convertToBase BAE.Base64 bs :: ByteString)

-- | Decode unpadded standard base64.
argon2B64Decode :: T.Text -> Maybe ByteString
argon2B64Decode t =
  let padLen = (4 - T.length t `mod` 4) `mod` 4
      padded = TE.encodeUtf8 (t <> T.replicate padLen "=")
   in either (const Nothing) Just (BAE.convertFromBase BAE.Base64 padded)

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
    sha256Bytes bs = BA.convert (hash bs :: Digest SHA256)
