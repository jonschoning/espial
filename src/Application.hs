{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Application
  ( getApplicationDev,
    appMain,
    develMain,
    makeFoundation,
    makeLogWare,

    -- * for DevelMain
    getApplicationRepl,
    shutdownApp,

    -- * for GHCI
    handler,
    db,
  )
where

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Archiver.ArchiveBox07 (archiveBox07Backend)
import Archiver.Backend (ArchiveJob (..), ArchiveJobStore (..), ArchiverBackend, ArchiverDB (..), newArchiveQueue, runArchiveQueueWorker)
import Archiver.Debug (debugArchiverBackend)
import Archiver.WaybackMachine (waybackMachineBackend)
import Control.Monad.Logger
import Data.Aeson qualified as A
import Database.Persist.Sqlite (ConnectionPool, createSqlitePoolFromInfo, fkEnabled, mkSqliteConnectionInfo, runSqlPool, sqlDatabase, sqlPoolSize, extraPragmas)
import Handler.AccountSettings
import Handler.Add
import Handler.Archive
import Handler.Common
import Handler.Docs
import Handler.Edit
import Handler.Home
import Handler.Locales
import Handler.Notes
import Handler.User
import I18n qualified
import Import
import Language.Haskell.TH.Syntax (qLocation)
import Lens.Micro
import Model.Migrations (runPreMigrations, runPersistentMigrations, runAppMigrations)
import Network.Connection qualified as NC
import Network.HTTP.Client.TLS
import Network.HTTP.Client.TLS qualified as NHT
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, defaultShouldDisplayException, getPort, runSettings, setHost, setOnException, setPort)
import Network.Wai.Handler.WarpTLS (TLSSettings (..), runTLS, tlsSettings)
import Network.TLS (Credentials (..), ServerHooks (..))
import Network.TLS qualified as TLS
import Control.Concurrent (forkIO, killThread, threadDelay)
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Gzip hiding (def)
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource (..), OutputFormat (..), destination, mkRequestLogger, outputFormat)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Version qualified as Version

#ifndef mingw32_HOST_OS
import System.Posix.Signals qualified as PS (Handler (Catch, CatchOnce), installHandler, sigHUP, sigTERM)
import Control.Concurrent qualified as CC (killThread, myThreadId)
#endif

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings@AppSettings {..} = do
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appFrontendBundleName <- loadFrontendBundleName appStaticDir
  appStatic <- (if appMutableStatic then staticDevel else static) appStaticDir
  (appTranslationsHash, appTranslations) <- I18n.loadTranslations appStaticDir
  appPublicTagCloudCache <- newIORef mempty
  appLoginRateLimiter <- newIORef mempty
  appDBWriteLock <- newDBWriteLock
  let appTranslate lang = I18n.translate appTranslations lang (I18nNs "translation")
      appI18nR = LocalesFileR appTranslationsHash []
      mkFoundation appConnPool appArchiver appArchiveWorkerThreadId = App {..}
      appLogFunc = messageLoggerSource (mkFoundation (error "connPool forced in tempFoundation") Nothing Nothing) appLogger
      startupLogFunc = if appEnableStartupLogging then appLogFunc else \_ _ _ _ -> pure ()
  flip runLoggingT startupLogFunc $ do 
    (logInfoNS "startup" Version.versionSpec)
  do migrationsPool <- mkPool startupLogFunc False
     runSqlPool (runPreMigrations appEnableStartupLogging) migrationsPool
     runSqlPool (runPersistentMigrations appEnableStartupLogging) migrationsPool
     runSqlPool (runAppMigrations appEnableStartupLogging) migrationsPool
  appPool <- mkPool appLogFunc True
  let archiverDB =
        ArchiverDB
          { archiverRunDB = \action -> runSqlPool action appPool,
            archiverRunDBWrite = \action -> withDBWriteLock appSqliteAppWriteLock appDBWriteLock (runSqlPool action appPool)
          }
  archiverBackend <- mkArchiverBackend appLogFunc archiverDB
  let archiveJobStore =
        ArchiveJobStore
          { archiveJobStoreInsertMany = \jobs ->
              archiverRunDBWrite archiverDB (insertArchiveJobRecords [(userId, bid, unUrl url) | (userId, bid, url) <- jobs]),
            archiveJobStoreDelete = \jobId ->
              archiverRunDBWrite archiverDB (deleteArchiveJobRecord jobId),
            archiveJobStoreLoadAll =
              archiverRunDB archiverDB getArchiveJobRecords <&> map toQueuedJob,
            archiveJobStoreBookmarkExists = \bid ->
              archiverRunDB archiverDB (get bid) <&> isJust
          }
      toQueuedJob (Entity jobId ArchiveJobRecord {..}) =
        (jobId, ArchiveJob archiveJobRecordUserId archiveJobRecordBookmarkId (Url archiveJobRecordHref))
  archiveWorker <- forM archiverBackend $ \archiver -> do
    queue <- newArchiveQueue archiveJobStore appArchiveQueueCapacity
    workerThreadId <- forkIO $ runArchiveQueueWorker archiver queue (appArchiveRateLimitMs * 1000) $ \e ->
      flip runLoggingT appLogFunc ($(logError) $ "Archive worker: " <> tshow e)
    pure (archiver, queue, workerThreadId)
  let appArchiver = (\(archiver, queue, _) -> (archiver, queue)) <$> archiveWorker
      appArchiveWorkerThreadId = (\(_, _, tid) -> tid) <$> archiveWorker
  flip runLoggingT startupLogFunc $ do
    (logInfoNS "startup" $ "archive backend: " <> tshow appArchiveBackend)
    (logDebugNS "startup" ("language-default: " <> fromI18nLang' appLanguageDefault))
  pure (mkFoundation appPool appArchiver appArchiveWorkerThreadId)
  where
    mkPool :: _ -> Bool -> IO ConnectionPool
    mkPool logFunc isFkEnabled =
      flip runLoggingT logFunc $ do
        let dbPath = sqlDatabase appDatabaseConf
            poolSize = sqlPoolSize appDatabaseConf
            connInfo =
              mkSqliteConnectionInfo dbPath
                & set fkEnabled isFkEnabled
                & set extraPragmas ["PRAGMA busy_timeout = " <> tshow appSqliteBusyTimeoutMs <> ";"]
        createSqlitePoolFromInfo connInfo poolSize
    mkArchiverBackend :: _ -> ArchiverDB -> IO (Maybe ArchiverBackend)
    mkArchiverBackend logFunc archiverDB = do
      case appArchiveBackend of
        ArchiveBackendDisabled -> pure Nothing
        ArchiveBackendDebug -> pure (Just (debugArchiverBackend logFunc))
        ArchiveBackendArchiveLi -> mkArchiveLiArchiver
        ArchiveBackendWaybackMachine -> mkWaybackArchiver
        ArchiveBackendArchiveBox07 -> mkArchiveBox07Archiver
      where
        mkArchiveLiArchiver = do
          flip runLoggingT logFunc ($(logWarn) ("Archive backend `archive-li` selected but functionality has been removed; archiving disabled"))
          pure Nothing
        mkWaybackArchiver = do
          case (appWaybackMachineAccessKey, appWaybackMachineSecretKey) of
            (Just accessKey, Just secretKey) -> do
              archiveManager <- do
                let mSocks = NC.SockSettingsSimple <$> fmap unpack appArchiveSocksProxyHost <*> fmap toEnum appArchiveSocksProxyPort
                NHT.newTlsManagerWith (NHT.mkManagerSettings def mSocks)
              pure $ Just $ waybackMachineBackend accessKey secretKey archiverDB archiveManager espialUserAgent logFunc
            _ -> do
              flip runLoggingT logFunc ($(logWarn) ("Archive backend `wayback-machine` selected but access/secret key missing; archiving disabled"))
              pure Nothing
        mkArchiveBox07Archiver = archiveBox07Backend appSettings archiverDB logFunc

loadFrontendBundleName :: FilePath -> IO Text
loadFrontendBundleName staticDir = do
  let manifestPath = staticDir <> "/js/manifest.json"
      fallbackName = "app.min.js"
  mManifest <- A.decodeFileStrict' manifestPath
  pure $ maybe fallbackName appBundle mManifest

newtype FrontendBundleManifest = FrontendBundleManifest
  { appBundle :: Text
  }

instance A.FromJSON FrontendBundleManifest where
  parseJSON = A.withObject "FrontendBundleManifest" $ \o -> do
    appBundle <- o A..: "appBundle"
    pure FrontendBundleManifest {..}

makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  pure (logWare (makeMiddleware appPlain))

makeMiddleware :: Middleware
makeMiddleware =
  acceptOverride
    . autohead
    . gzip def {gzipFiles = GzipPreCompressed GzipIgnore}
    . methodOverride

makeLogWare :: App -> IO Middleware
makeLogWare foundation
  | appEnableRequestLogging (appSettings foundation) =
      mkRequestLogger
        def
          { outputFormat =
              if appDetailedRequestLogging (appSettings foundation)
                then Detailed True
                else
                  Apache
                    ( if appIpFromHeader (appSettings foundation)
                        then FromFallback
                        else FromSocket
                    ),
            destination = Logger (loggerSet (appLogger foundation))
          }
  | otherwise = pure id

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort (appSettings foundation))
    $ setHost (appHost (appSettings foundation))
    $ setOnException
      ( \_req e ->
          when (defaultShouldDisplayException e)
            $ messageLoggerSource
              foundation
              (appLogger foundation)
              $(qLocation >>= liftLoc)
              "yesod"
              LevelError
              (toLogStr $ "Exception from Warp: " ++ show e)
      )
      defaultSettings

makeTlsSettings :: FilePath -> FilePath -> IO TLSSettings
makeTlsSettings certPath keyPath = do
  creds <- loadCreds
  credsRef <- newIORef creds
  void $ forkIO $ forever $ do
    threadDelay reloadCredsFrequency
    reloadCreds credsRef
#ifndef mingw32_HOST_OS
  void $ PS.installHandler PS.sigHUP (PS.Catch $ reloadCreds credsRef) Nothing
#endif
  pure (tlsSettings certPath keyPath)
    { tlsServerHooks = def { onServerNameIndication = \_ -> readIORef credsRef } }
  where
    reloadCredsFrequency = 12 * 60 * 60 * 1_000_000 -- 12 hours in microseconds
    loadCreds =
      TLS.credentialLoadX509 certPath keyPath >>= \case
        Left e  -> throwString $ "TLS credential load failed: " ++ e
        Right c -> pure $ Credentials [c]
    reloadCreds ref =
      tryAny loadCreds >>= \case
        Left e  -> putStrLn $ "TLS cert reload failed: " <> tshow e
        Right c -> writeIORef ref c

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings (warpSettings foundation)
  app <- makeApplication foundation
  pure (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [configSettingsYmlValue] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  settings <- getAppSettings
  foundation <- makeFoundation settings
  app <- makeApplication foundation
#ifndef mingw32_HOST_OS
  mainThreadId <- CC.myThreadId
  void $ PS.installHandler PS.sigTERM (PS.CatchOnce (CC.killThread mainThreadId)) Nothing
#endif
  case (appTLSCertFile (appSettings foundation), appTLSKeyFile (appSettings foundation)) of
    (Just certFile, Just keyFile) -> do
      tlsS <- makeTlsSettings certFile keyFile
      flip runLoggingT (messageLoggerSource foundation (appLogger foundation)) (logInfoNS "startup" "starting espial server [tls]")
      runTLS tlsS (warpSettings foundation) app
    _ -> do
      flip runLoggingT (messageLoggerSource foundation (appLogger foundation)) (logInfoNS "startup" "starting espial server [http]")
      runSettings (warpSettings foundation) app

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings (warpSettings foundation)
  app1 <- makeApplication foundation
  pure (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp App {appArchiveWorkerThreadId} = forM_ appArchiveWorkerThreadId killThread

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
