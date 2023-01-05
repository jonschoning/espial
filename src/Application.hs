{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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
import           Database.Persist.Sqlite (ConnectionPool, mkSqliteConnectionInfo, createSqlitePoolFromInfo, fkEnabled, runSqlPool, sqlDatabase, sqlPoolSize)
import           Import
import           Language.Haskell.TH.Syntax (qLocation)
import           Lens.Micro
import           Network.HTTP.Client.TLS
import           Network.Wai (Middleware)
import           Network.Wai.Handler.Warp (Settings, defaultSettings, defaultShouldDisplayException, runSettings, setHost, setOnException, setPort, getPort)
import           Network.Wai.Middleware.AcceptOverride
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.MethodOverride
import           Network.Wai.Middleware.RequestLogger (Destination(Logger), IPAddrSource(..), OutputFormat(..), destination, mkRequestLogger, outputFormat)
import           System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
#ifndef mingw32_HOST_OS
import qualified Control.Concurrent as CC (killThread, myThreadId)
import qualified System.Posix.Signals as PS (installHandler, Handler(CatchOnce), sigTERM)
#endif

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
  appStatic <-
    (if appMutableStatic appSettings
       then staticDevel
       else static)
      (appStaticDir appSettings)
  let mkFoundation appConnPool = App {..}
      tempFoundation = mkFoundation (error "connPool forced in tempFoundation")
      logFunc = messageLoggerSource tempFoundation appLogger
  pool <- mkPool logFunc True
  poolMigrations <- mkPool logFunc False
  runLoggingT (runSqlPool runMigrations poolMigrations) logFunc
  return (mkFoundation pool)
  where
    mkPool :: _ -> Bool -> IO ConnectionPool
    mkPool logFunc isFkEnabled =
      flip runLoggingT logFunc $ do
        let dbPath = sqlDatabase (appDatabaseConf appSettings)
            poolSize = sqlPoolSize (appDatabaseConf appSettings)
            connInfo = mkSqliteConnectionInfo dbPath &
                       set fkEnabled isFkEnabled
        createSqlitePoolFromInfo connInfo poolSize


makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return (logWare (makeMiddleware appPlain))

makeMiddleware :: Middleware
makeMiddleware =
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
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
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
