{-# LANGUAGE ScopedTypeVariables #-}

module Model.Migrations where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import Control.Monad.Writer (tell)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Database.Persist.Sql
import GraphUtils (getPathValues)
import Model
import Types

type AppVersion = Int

newtype CurrentVersion = CurrentVersion AppVersion

newtype LatestMigrationVersion = LatestMigrationVersion AppVersion

type AppVersionPath = (AppVersion, AppVersion)

(~>) :: AppVersion -> AppVersion -> AppVersionPath
(~>) = (,)

data SqlStatement = SqlStatement
  { sqlText :: Text,
    sqlVals :: [PersistValue]
  }
  deriving (Show)

type OperationSpec m = (Text, SqlPersistT m [SqlStatement])

data AppOperationPath m = AppVersionPath := [OperationSpec m]

type AppMigrations m = [AppOperationPath m]

migrateAll :: Migration
migrateAll = migrateSchema

dumpMigration :: DB ()
dumpMigration = do
  printMigration migrateAll
  printAppMigrations

runMigrations :: DB ()
runMigrations = do
  runMigration migrateAll
  runAppMigrations

toMigration :: [Text] -> Migration
toMigration = lift . tell . fmap (False,)

appMigrations :: (MonadUnliftIO m) => AppMigrations m
appMigrations =
  [ 0 ~> 1 := [operation_create_initial_indexes],
    1 ~> 2 := [operation_normalize_bookmark_utctime, operation_normalize_note_utctime]
  ]

printAppMigrations :: forall m. DBM m ()
printAppMigrations = do
  (mcv, LatestMigrationVersion lmv) <- getVersions
  let CurrentVersion cv = fromMaybe (CurrentVersion 0) mcv
  if (cv >= lmv)
    then pure () -- putStrLn "No appMigrations to apply."
    else do
      putStrLn ("appMigration needed: current version " <> tshow cv <> ", latest version " <> tshow lmv)
      case getOperations appMigrations (Just cv) of
        Left (current, target) ->
          putStrLn (msg_migration_path_not_found current target)
        Right (_, endv, _) | cv >= endv -> do
          pure () -- putStrLn ("No appMigrations to apply. " <> if cv > endv then " (database version is newer than latest appMigrations version)" else "")
        Right (startv, endv, ops) -> do
          let strPath = tshow startv <> " ~> " <> tshow endv
              strOps = T.intercalate ", " (fmap fst ops)
          putStrLn ("Found appMigration path: " <> strPath <> " (" <> strOps <> ")")
          forM_ ops $ \(name, opSpec) -> do
            putStrLn ("*** Dumping migration operation: " <> name)
            statements <- opSpec
            traverse_ (\(SqlStatement sql vals) -> putStrLn (sql <> " " <> tshow vals)) statements
          pure ()

runAppMigrations :: forall m. DBM m ()
runAppMigrations = do
  (mcv, LatestMigrationVersion lmv) <- getVersions
  case mcv of
    Nothing -> do
      putStrLn ("table 'app_migration' does not exist. aborting appMigrations")
      pure ()
    Just (CurrentVersion cv) -> do
      if (cv >= lmv)
        then pure ()
        else do
          putStrLn ("appMigration needed: current version " <> tshow cv <> ", latest version " <> tshow lmv)
          case getOperations appMigrations (Just cv) of
            Left (current, target) ->
              putStrLn (msg_migration_path_not_found current target)
            Right (_, endv, _) | cv >= endv -> do
              pure () -- putStrLn ("No appMigrations to apply. " <> if cv > endv then " (database version is newer than latest appMigrations version)" else "")
            Right (startv, endv, ops) -> do
              let strPath = tshow startv <> " ~> " <> tshow endv
                  strOps = T.intercalate ", " (fmap fst ops)
              putStrLn ("Found appMigration path: " <> strPath <> " (" <> strOps <> ")")
              forM_ ops $ \(name, opSpec) -> do
                putStrLn ("Applying migration operation: " <> name)
                statements <- opSpec
                traverse_ (\(SqlStatement sql vals) -> rawExecute sql vals) statements
              now <- liftIO getCurrentTime
              insert_ (AppMigration endv (strPath <> ": " <> strOps) now)
              putStrLn ("appMigration complete: " <> strPath)
              pure ()

msg_migration_path_not_found :: AppVersion -> AppVersion -> Text
msg_migration_path_not_found current target =
  "Cannot migrate from current version "
    <> tshow current
    <> " to "
    <> tshow target
    <> ". No path of appMigrations found."

getVersions :: forall m. DBM m ((Maybe CurrentVersion, LatestMigrationVersion))
getVersions = do
  rawSql "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'app_migration'" [] >>= \case
    _p :: [Single Int]
      | null _p ->
          pure (Nothing, latestMigrationsVersion)
    _ -> do
      applied <- selectList [] [Asc AppMigrationVersion]
      let appliedVersions = fmap (appMigrationVersion . entityVal) applied
          currentVersion = CurrentVersion $ maybe 0 maximum (fromNullable appliedVersions)
      pure (Just currentVersion, latestMigrationsVersion)
  where
    latestMigrationsVersion = LatestMigrationVersion $ getLatestVersion (appMigrations :: AppMigrations m)

getOperations :: AppMigrations m -> Maybe AppVersion -> Either (AppVersion, AppVersion) (AppVersion, AppVersion, [OperationSpec m])
getOperations migration mVersion =
  case getPathValues edges start end of
    Just pathValues -> Right (start, end, concat pathValues)
    Nothing -> Left (start, end)
  where
    edges = map (\(path := ops) -> (path, ops)) migration
    start = fromMaybe (getFirstVersion migration) mVersion
    end = getLatestVersion migration
    getFirstVersion = maybe 0 minimum . fromNullable . map (\((v1, _) := _) -> v1)

getLatestVersion :: AppMigrations m -> AppVersion
getLatestVersion = maybe 0 maximum . fromNullable . map (\((_, v2) := _) -> v2)

operation_create_initial_indexes :: (Applicative m) => OperationSpec m
operation_create_initial_indexes =
  ("ensure-base-indexes",)
    $ pure
    $ flip SqlStatement []
    <$> [ "CREATE INDEX IF NOT EXISTS idx_bookmark_time ON bookmark (user_id, time DESC)",
          "CREATE INDEX IF NOT EXISTS idx_bookmark_tag_bookmark_id ON bookmark_tag (bookmark_id, id, tag, seq)",
          "CREATE INDEX IF NOT EXISTS idx_note_user_created ON note (user_id, created DESC)"
        ]

operation_normalize_bookmark_utctime :: (Applicative m) => OperationSpec m
operation_normalize_bookmark_utctime =
  ("normalize-bookmark-utctime",)
    $ pure
    $ flip SqlStatement []
    <$> filter (not . T.null . T.strip) (T.splitOn ";" migrationText)
  where
    migrationText = decodeUtf8 $(embedFile "appmigrations/002-normalize-bookmark-utctime.sql")

operation_normalize_note_utctime :: (Applicative m) => OperationSpec m
operation_normalize_note_utctime =
  ("normalize-note-utctime",)
    $ pure
    $ flip SqlStatement []
    <$> filter (not . T.null . T.strip) (T.splitOn ";" migrationText)
  where
    migrationText = decodeUtf8 $(embedFile "appmigrations/002-normalize-note-utctime.sql")
