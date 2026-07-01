{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestImport
  ( module TestImport,
    module X,
  )
where

import Application (makeFoundation, makeLogWare)
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, rawExecute, rawSql, runSqlPersistMPool, unSingle)
import Foundation as X
import Model as X
import Model.Migrations (runAppMigrations, runPersistentMigrations)
import Test.Hspec as X
import Types
import Yesod.Auth as X
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
  pool <- fmap appConnPool getTestYesod
  liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
  app <- getTestYesod
  fakeHandlerGetLogger appLogger app handler

-- Migrations run once for the whole suite; data is wiped between each test.
withApp :: SpecWith (TestApp App) -> Spec
withApp spec =
  beforeAll makeApp $
    beforeWith perTest spec
  where
    makeApp :: IO App
    makeApp = do
      settings <-
        loadYamlSettings
          ["config/test-settings.yml", "config/settings.yml"]
          []
          useEnv
      foundation <- makeFoundation settings
      runSqlPersistMPool
        (runPersistentMigrations False >> runAppMigrations False)
        (appConnPool foundation)
      return foundation

    perTest :: App -> IO (TestApp App)
    perTest foundation = do
      wipeDB foundation
      logWare <- makeLogWare foundation
      return (foundation, logWare)

-- Truncate all user data tables on the app's own pool, leaving app_migration
-- intact.  defer_foreign_keys lets us delete in any order within the
-- transaction; at commit time all tables are empty so no FK violations occur.
wipeDB :: App -> IO ()
wipeDB app =
  runSqlPersistMPool go (appConnPool app)
  where
    go :: SqlPersistM ()
    go = do
      rawExecute "PRAGMA defer_foreign_keys = ON" []
      tables <- getTables
      forM_ tables (\t -> rawExecute ("DELETE FROM " ++ t) [])

    getTables :: DB [Text]
    getTables = do
      tables <-
        rawSql
          "SELECT name FROM sqlite_master WHERE type = 'table' AND name != 'app_migration';"
          []
      return (fmap unSingle tables)
