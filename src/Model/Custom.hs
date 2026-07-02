module Model.Custom where

-- import Crypto.Argon2 qualified as Argon2
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
-- import Data.Text.Short qualified as TS
-- import Data.Word (Word32)
import Database.Persist.Sql
import System.Entropy (getEntropy)
-- import Text.Read (readMaybe)
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

-- | Selects the top-level password hashing algorithm (and its parameters) to use for
-- newly-created/rehashed hashes. Verification of existing hashes is unaffected -- it
-- always dispatches on the stored hash's own prefix, regardless of this selection.
data HashAlgoConfig
  = HashAlgoBCrypt HashingPolicy

--  | HashAlgoArgon2id Argon2.HashOptions

-- | Hash a password with BCrypt. Retained for testing BCrypt→Argon2id migration paths.
hashPasswordBCrypt :: T.Text -> IO PasswordHash
hashPasswordBCrypt = hashPasswordWith (HashAlgoBCrypt bcryptPolicy)

-- | True when the stored hash should be rehashed under 'targetAlgo' -- either because
-- it's a different algorithm, or (for Argon2id) because its embedded parameters no
-- longer match (e.g. after an AppSettings change).
needsRehash :: HashAlgoConfig -> PasswordHash -> Bool
needsRehash (HashAlgoBCrypt _) (PasswordHash stored) = not ("$2" `T.isPrefixOf` stored)

-- needsRehash (HashAlgoArgon2id targetOpts) (PasswordHash stored) =
-- storedArgon2Params stored /= Just (argon2ParamsOf targetOpts)

-- argon2ParamsOf :: Argon2.HashOptions -> (Word32, Word32, Word32)
-- argon2ParamsOf o = (Argon2.hashMemory o, Argon2.hashIterations o, Argon2.hashParallelism o)

-- | Extract the (memory, iterations, parallelism) triple embedded in a stored Argon2id
-- PHC-format hash, without needing to reconstruct a full 'Argon2.HashOptions' or touch
-- the salt/digest -- used only to decide whether 'needsRehash' should trigger.
-- storedArgon2Params :: T.Text -> Maybe (Word32, Word32, Word32)
-- storedArgon2Params stored = do
--   rest <- T.stripPrefix "$argon2id$v=19$m=" stored
--   case T.splitOn "$" rest of
--     (paramStr : _) -> case T.splitOn "," paramStr of
--       [mStr, tPart, pPart] ->
--         (,,)
--           <$> readMaybe (T.unpack mStr)
--           <*> (readMaybe . T.unpack =<< T.stripPrefix "t=" tPart)
--           <*> (readMaybe . T.unpack =<< T.stripPrefix "p=" pPart)
--       _ -> Nothing
--     _ -> Nothing

-- mkArgon2idOptions :: Int -> Int -> Int -> Argon2.HashOptions
-- mkArgon2idOptions memoryKib iterations parallelism =
--   Argon2.HashOptions
--     { Argon2.hashIterations = fromIntegral iterations,
--       Argon2.hashMemory = fromIntegral memoryKib,
--       Argon2.hashParallelism = fromIntegral parallelism,
--       Argon2.hashVariant = Argon2.Argon2id,
--       Argon2.hashVersion = Argon2.Argon2Version13,
--       Argon2.hashLength = 32
--     }

-- | OWASP's floor recommendation: m=19456 KiB, t=2, p=1.
-- defaultArgon2idOptions :: Argon2.HashOptions
-- defaultArgon2idOptions = mkArgon2idOptions 19456 2 1

-- | Hash a password with Argon2id using 'defaultArgon2idOptions'.
-- hashPassword :: T.Text -> IO PasswordHash
-- hashPassword = hashPasswordWith (HashAlgoArgon2id defaultArgon2idOptions)

-- | Hash a password under the given algorithm/parameters, producing a PHC-format string.
hashPasswordWith :: HashAlgoConfig -> T.Text -> IO PasswordHash
hashPasswordWith (HashAlgoBCrypt policy) pwd = do
  mbs <- hashPasswordUsingPolicy policy (TE.encodeUtf8 pwd)
  case mbs of
    Nothing -> ioError (userError "bcrypt: invalid hashing policy")
    Just bs -> pure (PasswordHash (TE.decodeUtf8 bs))

-- hashPasswordWith (HashAlgoArgon2id opts) pwd = do
--   salt <- getEntropy 16
--   case Argon2.hashEncoded opts (TE.encodeUtf8 pwd) salt of
--     Left err -> ioError (userError ("argon2: " <> show err))
--     Right encoded -> pure (PasswordHash (TS.toText encoded))

-- | Validate a plaintext password against a stored hash.
-- Dispatches to the correct algorithm based on the hash prefix.
validatePasswordHash :: PasswordHash -> T.Text -> Bool
validatePasswordHash ph@(PasswordHash h) pwd
  | "$2" `T.isPrefixOf` h = validateBCrypt ph pwd
  -- \| "$argon2id$" `T.isPrefixOf` h = validateArgon2id ph pwd
  | otherwise = False

validateBCrypt :: PasswordHash -> T.Text -> Bool
validateBCrypt (PasswordHash h) pwd =
  validatePassword (TE.encodeUtf8 h) (TE.encodeUtf8 pwd)

-- validateArgon2id :: PasswordHash -> T.Text -> Bool
-- validateArgon2id (PasswordHash stored) pwd =
--   Argon2.verifyEncoded (TS.fromText stored) (TE.encodeUtf8 pwd) == Argon2.Argon2Ok

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
