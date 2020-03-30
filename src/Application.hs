{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( getApplicationDev
  , appMain
  , develMain
  , makeFoundation
  , makeLogWare
   -- * for DevelMain
  , getApplicationRepl
  , shutdownApp
   -- * for GHCI
  , handler
  , db
  ) where

import           Control.Monad.Logger (liftLoc, runLoggingT)
import           Database.Persist.Sqlite (createSqlitePool, runSqlPool, sqlDatabase, sqlPoolSize)
import           Import
import           Language.Haskell.TH.Syntax (qLocation)
-- import           Lens.Micro
import           Network.HTTP.Client.TLS
import           Network.Wai (Middleware)
import           Network.Wai.Handler.Warp (Settings, defaultSettings, defaultShouldDisplayException, runSettings, setHost, setOnException, setPort, getPort)
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.MethodOverride
import           Network.Wai.Middleware.RequestLogger (Destination(Logger), IPAddrSource(..), OutputFormat(..), destination, mkRequestLogger, outputFormat)
import           System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)

-- import qualified Control.Monad.Metrics as MM
-- import qualified Network.Wai.Metrics as WM
-- import qualified System.Metrics as EKG
-- import qualified System.Remote.Monitoring as EKG

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Common
import           Handler.Home
import           Handler.User
import           Handler.AccountSettings
import           Handler.Add
import           Handler.Edit
import           Handler.Notes
import           Handler.Docs

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- getGlobalManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  -- store <- EKG.newStore
  -- EKG.registerGcMetrics store
  -- appMetrics <- MM.initializeWith store
  appStatic <-
    (if appMutableStatic appSettings
       then staticDevel
       else static)
      (appStaticDir appSettings)
  let mkFoundation appConnPool = App { ..}
      tempFoundation = mkFoundation (error "connPool forced in tempFoundation")
      logFunc = messageLoggerSource tempFoundation appLogger
  pool <-
    flip runLoggingT logFunc $
    createSqlitePool
      (sqlDatabase (appDatabaseConf appSettings))
      (sqlPoolSize (appDatabaseConf appSettings))
  runLoggingT
    (runSqlPool runMigrations pool)
    logFunc
  return (mkFoundation pool)

makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  -- let store = appMetrics foundation ^. MM.metricsStore
  -- waiMetrics <- WM.registerWaiMetrics store
  return (logWare (makeMiddleware appPlain))

makeMiddleware :: Middleware
makeMiddleware =
  -- WM.metrics waiMetrics .
  acceptOverride .
  autohead .
  gzip def {gzipFiles = GzipPreCompressed GzipIgnore} .
  methodOverride

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
    { outputFormat =
      if appDetailedRequestLogging (appSettings foundation)
        then Detailed True
        else Apache
               (if appIpFromHeader (appSettings foundation)
                  then FromFallback
                  else FromSocket)
    , destination = Logger (loggerSet (appLogger foundation))
    }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort (appSettings foundation)) $
  setHost (appHost (appSettings foundation)) $
  setOnException
    (\_req e ->
        when (defaultShouldDisplayException e) $
        messageLoggerSource
          foundation
          (appLogger foundation)
          $(qLocation >>= liftLoc)
          "yesod"
          LevelError
          (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings (warpSettings foundation)
  app <- makeApplication foundation
  -- forkEKG foundation
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- forkEKG :: App -> IO ()
-- forkEKG foundation = 
--   let settings = appSettings foundation in
--   for_ (appEkgHost settings) $ \ekgHost ->
--     for_ (appEkgPort settings) $ \ekgPort ->
--       EKG.forkServerWith
--         (appMetrics foundation ^. MM.metricsStore)
--         (encodeUtf8 ekgHost)
--         ekgPort

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  foundation <- makeFoundation settings
  app <- makeApplication foundation
  -- forkEKG foundation
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
