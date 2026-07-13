-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception qualified as Exception
import Control.Monad.Fail (fail)
import Data.Aeson
  ( Result (..),
    encode,
    fromJSON,
    withObject,
    withText,
    (.!=),
    (.:?),
  )
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Sqlite (SqliteConf)
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Model
import Model.Custom (HashAlgoConfig (..), bcryptPolicy)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util
  ( WidgetFileSettings,
    widgetFileNoReload,
    widgetFileReload,
  )

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
  { -- | Directory from which to serve static files.
    appStaticDir :: String,
    -- | Configuration settings for accessing the database.
    appDatabaseConf :: SqliteConf,
    -- | Serialize all DB write transactions through a single in-process lock, to
    -- avoid SQLite `SQLITE_BUSY`/snapshot conflicts under concurrent writers.
    appSqliteAppWriteLock :: Bool,
    -- | SQLite @busy_timeout@, in milliseconds: how long a connection retries
    -- before returning `SQLITE_BUSY` on lock contention.
    appSqliteBusyTimeoutMs :: Int,
    -- | Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    appRoot :: Maybe Text,
    -- | Host/interface the server should bind to.
    appHost :: HostPreference,
    -- | Port to listen on
    appPort :: Int,
    -- | Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    appIpFromHeader :: Bool,
    -- | Use detailed request logging system
    appDetailedRequestLogging :: Bool,
    -- | Enable request logging middleware.
    appEnableRequestLogging :: Bool,
    -- | Should all log messages be displayed?
    appShouldLogAll :: Bool,
    -- | Enable startup and migration log output.
    appEnableStartupLogging :: Bool,
    -- | Use the reload version of templates
    appReloadTemplates :: Bool,
    -- | Assume that files in the static dir may change after compilation
    appMutableStatic :: Bool,
    -- | Perform no stylesheet/script combining
    appSkipCombining :: Bool,
    -- Example app-specific configuration values.

    -- | Copyright text to appear in the footer of the page
    appCopyright :: Text,
    -- | Google Analytics code
    appAnalytics :: Maybe Text,
    -- | Indicate if auth dummy login should be enabled.
    appAuthDummyLogin :: Bool,
    -- | Socks proxy host to use when making archive requests
    appArchiveSocksProxyHost :: Maybe Text,
    -- | Socks proxy port to use when making archive requests
    appArchiveSocksProxyPort :: Maybe Int,
    -- | Wayback Machine access key for archive requests
    appWaybackMachineAccessKey :: Maybe Text,
    -- | Wayback Machine secret key for archive requests
    appWaybackMachineSecretKey :: Maybe Text,
    -- | ArchiveBox-07 base URL used for web UI login and submissions
    appArchiveBoxUrl :: Maybe Text,
    -- | Public ArchiveBox-07 base URL used for bookmark links
    appArchiveBoxPublicUrl :: Maybe Text,
    -- | ArchiveBox-07 admin username used for web UI login
    appArchiveBoxUsername :: Maybe Text,
    -- | ArchiveBox-07 admin password used for web UI login
    appArchiveBoxPassword :: Maybe Text,
    -- | Tag applied to URLs submitted via the ArchiveBox-07 backend
    appArchiveBoxTag :: Text,
    -- | Optional comma-separated ArchiveBox-07 archive method override
    appArchiveBoxPlugins :: Maybe Text,
    -- | Which archiver backend to use (or disabled)
    appArchiveBackend :: ArchiveBackend,
    -- | Uri to app source code
    appSourceCodeUri :: Maybe Text,
    -- | Whether to only allow SSL connections (i.e. disable non-https cookies and redirects)
    appSSLOnly :: Bool,
    -- | Whether to allow non-http URL schemes (e.g. mailto:, ipfs:, etc.) in user input.
    appAllowNonHttpUrlSchemes :: Bool,
    -- | Default language for the application
    appLanguageDefault :: I18nLang,
    -- | Path to TLS certificate file; enables TLS when set with appTLSKeyFile
    appTLSCertFile :: Maybe FilePath,
    -- | Path to TLS private key file; enables TLS when set with appTLSCertFile
    appTLSKeyFile :: Maybe FilePath,
    -- | TTL in seconds for the public tag cloud response cache
    appPublicTagCloudCacheDurationSeconds :: Int,
    -- | Which password hashing algorithm to use for newly-created/rehashed password hashes.
    appPasswordHashAlgo :: PasswordHashAlgo,
    -- checked before the password hash is computed.
    appLoginRateLimitMaxAttempts :: Int,
    -- | Login rate-limit window, in seconds.
    appLoginRateLimitWindowSeconds :: Int,
    -- | Maximum allowed request body size, in bytes (e.g. for Settings/import).
    -- @0@ disables the limit.
    appMaximumContentLength :: Int
  }

instance FromJSON AppSettings where
  parseJSON = withObject "AppSettings" \o -> do
    let defaultDev = 
#ifdef DEVELOPMENT
                True
#else
                False
#endif
    appStaticDir <- o .: "static-dir"
    appDatabaseConf <- o .: "database"
    appSqliteAppWriteLock <- o .:? "sqlite-app-write-lock" .!= True
    appSqliteBusyTimeoutMs <- o .:? "sqlite-busy-timeout-ms" .!= 30000
    appRoot <- o .:? "approot"
    appHost <- fromString <$> o .: "host"
    appPort <- o .: "port"
    appIpFromHeader <- o .: "ip-from-header"

    dev <- o .:? "development" .!= defaultDev

    appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
    appEnableRequestLogging <- o .:? "request-logging" .!= True
    appShouldLogAll <- o .:? "should-log-all" .!= dev
    appEnableStartupLogging <- o .:? "startup-logging" .!= True
    appReloadTemplates <- o .:? "reload-templates" .!= dev
    appMutableStatic <- o .:? "mutable-static" .!= dev
    appSkipCombining <- o .:? "skip-combining" .!= dev

    appCopyright <- o .: "copyright"
    appAnalytics <- o .:? "analytics"

    appAuthDummyLogin <- o .:? "auth-dummy-login" .!= dev

    appArchiveBackend <- o .:? "archive-backend" .!= ArchiveBackendDisabled

    appArchiveSocksProxyHost <- o .:? "archive-socks-proxy-host"
    appArchiveSocksProxyPort <- o .:? "archive-socks-proxy-port"

    appWaybackMachineAccessKey <- fmap toText <$> o .:? "wayback-machine-access-key"
    appWaybackMachineSecretKey <- fmap toText <$> o .:? "wayback-machine-secret-key"

    appArchiveBoxUrl <- o .:? "archivebox-url"
    appArchiveBoxPublicUrl <- o .:? "archivebox-public-url"
    appArchiveBoxUsername <- fmap toText <$> o .:? "archivebox-username"
    appArchiveBoxPassword <- fmap toText <$> o .:? "archivebox-password"
    appArchiveBoxTag <- (fmap toText <$> o .:? "archivebox-tag") .!= "espial"
    appArchiveBoxPlugins <- o .:? "archivebox-plugins"

    appSourceCodeUri <- o .:? "source-code-uri"

    appSSLOnly <- fromMaybe False <$> o .:? "ssl-only"

    appAllowNonHttpUrlSchemes <- o .:? "allow-non-http-url-schemes" .!= False

    appLanguageDefault <- o .:? "language-default" .!= I18nLangEn

    appTLSCertFile <- o .:? "tls-cert-file"
    appTLSKeyFile <- o .:? "tls-key-file"

    appPublicTagCloudCacheDurationSeconds <- o .:? "public-tag-cloud-cache-duration-seconds" .!= 30

    appPasswordHashAlgo <- o .:? "password-hash-algo" .!= PasswordHashAlgoBCrypt

    appLoginRateLimitMaxAttempts <- o .:? "login-rate-limit-max-attempts" .!= 10
    appLoginRateLimitWindowSeconds <- o .:? "login-rate-limit-window-seconds" .!= 60

    appMaximumContentLength <- o .:? "maximum-content-length" .!= (2 * 1024 * 1024)

    pure AppSettings {..}
    where
      toText (String t) = t
      toText other = (decodeUtf8 . toStrict . encode) other

-- | Top-level password hashing algorithm selection.
data PasswordHashAlgo = PasswordHashAlgoBCrypt
  deriving (Show, Eq)

instance FromJSON PasswordHashAlgo where
  parseJSON = withText "PasswordHashAlgo" $ \case
    "bcrypt" -> pure PasswordHashAlgoBCrypt
    _ -> fail "Unknown password hash algorithm"

-- | Builds the password hashing configuration (algorithm + its parameters) used for
-- newly-created/rehashed password hashes, from the configured 'AppSettings'.
appPasswordHashConfig :: AppSettings -> HashAlgoConfig
appPasswordHashConfig AppSettings {..} =
  case appPasswordHashAlgo of
    PasswordHashAlgoBCrypt -> HashAlgoBCrypt bcryptPolicy

-- | Selects which archive backend is active.
data ArchiveBackend = ArchiveBackendDisabled | ArchiveBackendDebug | ArchiveBackendArchiveLi | ArchiveBackendWaybackMachine | ArchiveBackendArchiveBox07
  deriving (Show, Eq)

instance FromJSON ArchiveBackend where
  parseJSON = withText "ArchiveBackend" $ \case
    "disabled" -> pure ArchiveBackendDisabled
    "debug" -> pure ArchiveBackendDebug
    "archive-li" -> pure ArchiveBackendArchiveLi
    "wayback-machine" -> pure ArchiveBackendWaybackMachine
    "archivebox07" -> pure ArchiveBackendArchiveBox07
    _ -> fail "Unknown archive backend"

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile =
  ( if appReloadTemplates compileTimeAppSettings
      then widgetFileReload
      else widgetFileNoReload
  )
    widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue =
  either Exception.throw id
    $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
  case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
    Error e -> error e
    Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets =
  combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts =
  combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
