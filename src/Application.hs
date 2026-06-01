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

import Control.Monad.Logger (liftLoc, runLoggingT)
import qualified Data.Aeson as A
import Database.Persist.Sqlite (ConnectionPool, createSqlitePoolFromInfo, fkEnabled, mkSqliteConnectionInfo, runSqlPool, sqlDatabase, sqlPoolSize)
import Import
import Language.Haskell.TH.Syntax (qLocation)
import Lens.Micro
import qualified Network.Connection as NC
import Network.HTTP.Client.TLS
import qualified Network.HTTP.Client.TLS as NHT
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, defaultShouldDisplayException, getPort, runSettings, setHost, setOnException, setPort)
import Network.Wai.Middleware.AcceptOverride
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.Gzip hiding (def)
import Network.Wai.Middleware.MethodOverride
import Network.Wai.Middleware.RequestLogger (Destination (Logger), IPAddrSource (..), OutputFormat (..), destination, mkRequestLogger, outputFormat)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
#ifndef mingw32_HOST_OS
import qualified Control.Concurrent as CC (killThread, myThreadId)
import qualified System.Posix.Signals as PS (installHandler, Handler(CatchOnce), sigTERM)
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

import Archiver.ArchiveBox07 (archiveBox07Backend)
import Archiver.Debug (debugArchiverBackend)
import Archiver.Backend (ArchiverBackend)
import Archiver.WaybackMachine (waybackMachineBackend)
import Handler.AccountSettings
import Handler.Add
import Handler.Common
import Handler.Docs
import Handler.Edit
import Handler.Home
import Handler.Notes
import Handler.User
import Handler.Archive

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appFrontendBundleName <- loadFrontendBundleName (appStaticDir appSettings)
  appStatic <-
    ( if appMutableStatic appSettings
        then staticDevel
        else static
    )
      (appStaticDir appSettings)
  let mkFoundation appConnPool appArchiver = App {..}
      tempFoundation = mkFoundation (error "connPool forced in tempFoundation") Nothing
      logFunc = messageLoggerSource tempFoundation appLogger
  pool <- mkPool logFunc True
  poolMigrations <- mkPool logFunc False
  flip runLoggingT logFunc $ runSqlPool runMigrations poolMigrations
  appArchiver <- mkArchiverBackend logFunc pool
  flip
    runLoggingT
    logFunc
    ($(logInfo) $ "Archive backend: " <> tshow (appArchiveBackend appSettings))
  return (mkFoundation pool appArchiver)
  where
    mkPool :: _ -> Bool -> IO ConnectionPool
    mkPool logFunc isFkEnabled =
      flip runLoggingT logFunc $ do
        let dbPath = sqlDatabase (appDatabaseConf appSettings)
            poolSize = sqlPoolSize (appDatabaseConf appSettings)
            connInfo =
              mkSqliteConnectionInfo dbPath
                & set fkEnabled isFkEnabled
        createSqlitePoolFromInfo connInfo poolSize
    mkArchiverBackend :: _ -> ConnectionPool -> IO (Maybe ArchiverBackend)
    mkArchiverBackend logFunc pool = do
      case appArchiveBackend appSettings of
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
          case (appWaybackMachineAccessKey appSettings, appWaybackMachineSecretKey appSettings) of
            (Just accessKey, Just secretKey) -> do
              archiveManager <- do
                let mSocks = NC.SockSettingsSimple <$> fmap unpack (appArchiveSocksProxyHost appSettings) <*> fmap toEnum (appArchiveSocksProxyPort appSettings)
                NHT.newTlsManagerWith (NHT.mkManagerSettings def mSocks)
              pure $ Just $ waybackMachineBackend accessKey secretKey pool archiveManager logFunc
            _ -> do
              flip
                runLoggingT
                logFunc
                ($(logWarn) ("Archive backend `wayback-machine` selected but access/secret key missing; archiving disabled"))
              pure Nothing
        mkArchiveBox07Archiver = archiveBox07Backend appSettings pool logFunc

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
  return (logWare (makeMiddleware appPlain))

makeMiddleware :: Middleware
makeMiddleware =
  acceptOverride
    . autohead
    . gzip def {gzipFiles = GzipPreCompressed GzipIgnore}
    . methodOverride

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
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

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings (warpSettings foundation)
  app <- makeApplication foundation
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [configSettingsYmlValue] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  app <- makeApplication foundation
#ifndef mingw32_HOST_OS
  mainThreadId <- CC.myThreadId
  void $ PS.installHandler PS.sigTERM (PS.CatchOnce (CC.killThread mainThreadId)) Nothing
#endif
  runSettings (warpSettings foundation) app

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings (warpSettings foundation)
  app1 <- makeApplication foundation
  return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
