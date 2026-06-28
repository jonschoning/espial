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
import Control.Monad.Logger (runLoggingT)
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, rawExecute, rawSql, runSqlPersistMPool, unSingle)
import Database.Persist.Sqlite (createSqlitePoolFromInfo, fkEnabled, mkSqliteConnectionInfo, sqlDatabase)
import Foundation as X
import Lens.Micro (set)
import Model as X
import Model.Migrations (runAppMigrations, runPersistentMigrations)
import Settings (appDatabaseConf)
import Test.Hspec as X
import Types
import Yesod.Auth as X
import Yesod.Core (messageLoggerSource)
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

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <-
    loadYamlSettings
      ["config/test-settings.yml", "config/settings.yml"]
      []
      useEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
  -- Wipe data with FK disabled (for file-based DBs; no-op for :memory:).
  let logFunc = messageLoggerSource app (appLogger app)
      dbName = sqlDatabase $ appDatabaseConf $ appSettings app
      connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName
  pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc
  flip runSqlPersistMPool pool $ do
    tables <- getTables
    let queries = map (\t -> "DELETE FROM " ++ t) tables
    forM_ queries (\q -> rawExecute q [])
  -- Run migrations last so the pool tests use has the schema and tracking rows.
  runSqlPersistMPool (runPersistentMigrations False >> runAppMigrations False) (appConnPool app)

getTables :: DB [Text]
getTables = do
  tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
  return (fmap unSingle tables)
