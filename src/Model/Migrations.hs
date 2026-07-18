module Model.Migrations where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Database.Persist.Sql
import GraphUtils (getPathValues)
import Model
import Types
import Version qualified as V

type DbVersion = Int

newtype CurrentDbVersion = CurrentDbVersion DbVersion

newtype LatestMigrationVersion = LatestMigrationVersion DbVersion

type DbVersionPath = (DbVersion, DbVersion)

(~>) :: DbVersion -> DbVersion -> DbVersionPath
(~>) = (,)

data SqlStatement = SqlStatement {sql :: Text, vals :: [PersistValue]} deriving (Show)

type OperationSpec m = (Text, SqlPersistT m [SqlStatement])

data OperationPath m = DbVersionPath := [OperationSpec m]

type AppMigrations m = [OperationPath m]

dumpMigration :: DB ()
dumpMigration = do
  printMigration migrateSchema
  printAppMigrations

runPersistentMigrations :: Bool -> DB ()
runPersistentMigrations enableLogging = do
  if enableLogging
    then void $ runMigration migrateSchema
    else void $ runMigrationQuiet migrateSchema

runPreMigrations :: Bool -> DB ()
runPreMigrations enableLogging =
  runPreMigrations' (if enableLogging then liftIO . TIO.hPutStrLn stderr else const (pure ()))

-- runs before runPersistentMigrations, so the schema may not exist yet and
-- only raw SQL guarded by table-existence checks is safe here
runPreMigrations' :: forall m. (MonadUnliftIO m) => (Text -> m ()) -> DBM m ()
runPreMigrations' logger = do
  (mcv, _) <- getDbVersions True
  let cv = maybe 0 (\(CurrentDbVersion v) -> v) mcv
  when (cv <= 2) $ do
    noteTableExists <- rawSql "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'note'" []
    case noteTableExists :: [Single Int] of
      [] -> pure ()
      _ -> do
        lift (logger "Applying preMigration operation: dedupe-duplicate-note-slugs")
        rawExecute
          "UPDATE note SET slug = lower(hex(randomblob(10))) WHERE id NOT IN (SELECT MIN(id) FROM note GROUP BY user_id, slug)"
          []

runAppMigrations :: Bool -> DB ()
runAppMigrations enableLogging = do
  insertCurrentAppVersion
  runAppMigrations' (if enableLogging then liftIO . TIO.hPutStrLn stderr else const (pure ()))

insertCurrentAppVersion :: DB ()
insertCurrentAppVersion = do
  appVersion <- selectFirst [] [Desc AppVersionId]
  when (maybe True ((/= V.versionSpec) . appVersionAppVersionSpec . entityVal) appVersion) $ do
    now <- liftIO getCurrentTime
    insert_ (AppVersion V.versionSpec (pack V.appVersion) (pack V.gitShaShort) now)

appMigrations :: (MonadUnliftIO m) => AppMigrations m
appMigrations =
  [ 0 ~> 1 := [],
    1 ~> 2 := [operation_normalize_bookmark_utctime_remove_z, operation_normalize_note_utctime_remove_z],
    2 ~> 3 := [operation_marker_unique_user_note_slug]
  ]

-- run on every startup regardless of DbVersion, since migrateSchema may
-- recreate tables (dropping their indexes) at any time
alwaysAppMigrations :: (MonadUnliftIO m) => [OperationSpec m]
alwaysAppMigrations = [operation_drop_superseded_indexes, operation_create_initial_indexes]

printAppMigrations :: DB ()
printAppMigrations = do
  forM_ alwaysAppMigrations $ \(name, opSpec) -> do
    putStrLn ("*** Dumping always-run migration operation: " <> name)
    statements <- opSpec
    traverse_ (\(SqlStatement sql vals) -> putStrLn (sql <> " " <> tshow vals)) statements
  (mcv, LatestMigrationVersion lmv) <- getDbVersions True
  let CurrentDbVersion cv = fromMaybe (CurrentDbVersion 0) mcv
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

runAppMigrations' :: (MonadUnliftIO m) => (Text -> m ()) -> DBM m ()
runAppMigrations' logger = do
  let logFunc = lift . logger
  forM_ alwaysAppMigrations $ \(_, opSpec) -> do
    statements <- opSpec
    traverse_ (\(SqlStatement sql vals) -> rawExecute sql vals) statements
  (mcv, LatestMigrationVersion lmv) <- getDbVersions False
  case mcv of
    Nothing -> do
      logFunc "table 'app_migration' does not exist. aborting appMigrations"
      pure ()
    Just (CurrentDbVersion cv) -> do
      if (cv >= lmv)
        then pure ()
        else do
          logFunc ("appMigration needed: current version " <> tshow cv <> ", latest version " <> tshow lmv)
          case getOperations appMigrations (Just cv) of
            Left (current, target) ->
              logFunc (msg_migration_path_not_found current target)
            Right (_, endv, _) | cv >= endv -> do
              pure () -- putStrLn ("No appMigrations to apply. " <> if cv > endv then " (database version is newer than latest appMigrations version)" else "")
            Right (startv, endv, ops) -> do
              let strPath = tshow startv <> " ~> " <> tshow endv
                  strOps = T.intercalate ", " (fmap fst ops)
              logFunc ("Found appMigration path: " <> strPath <> " (" <> strOps <> ")")
              forM_ ops $ \(name, opSpec) -> do
                logFunc ("Applying migration operation: " <> name)
                statements <- opSpec
                traverse_ (\(SqlStatement sql vals) -> rawExecute sql vals) statements
              now <- liftIO getCurrentTime
              insert_ (AppMigration endv (strPath <> ": " <> strOps) (V.versionSpec) now)
              logFunc ("appMigration complete: " <> strPath)
              pure ()

msg_migration_path_not_found :: DbVersion -> DbVersion -> Text
msg_migration_path_not_found current target =
  "Cannot migrate from current version "
    <> tshow current
    <> " to "
    <> tshow target
    <> ". No path of appMigrations found."

getDbVersions :: forall m. Bool -> DBM m ((Maybe CurrentDbVersion, LatestMigrationVersion))
getDbVersions checkSchema = do
  (if checkSchema then (rawSql "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'app_migration'" []) else pure [Single 1]) >>= \case
    _p :: [Single Int]
      | null _p ->
          pure (Nothing, latestMigrationsVersion)
    _ -> do
      applied <- selectList [] [Asc AppMigrationDbVersion]
      let appliedVersions = fmap (appMigrationDbVersion . entityVal) applied
          currentDbVersion = CurrentDbVersion $ maybe 0 maximum (fromNullable appliedVersions)
      pure (Just currentDbVersion, latestMigrationsVersion)
  where
    latestMigrationsVersion = LatestMigrationVersion $ getLatestVersion (appMigrations :: AppMigrations m)

getOperations :: AppMigrations m -> Maybe DbVersion -> Either (DbVersion, DbVersion) (DbVersion, DbVersion, [OperationSpec m])
getOperations migration mVersion =
  case getPathValues edges start end of
    Just pathValues -> Right (start, end, concat pathValues)
    Nothing -> Left (start, end)
  where
    edges = map (\(path := ops) -> (path, ops)) migration
    start = fromMaybe (getFirstVersion migration) mVersion
    end = getLatestVersion migration
    getFirstVersion = maybe 0 minimum . fromNullable . map (\((v1, _) := _) -> v1)

getLatestVersion :: AppMigrations m -> DbVersion
getLatestVersion = maybe 0 maximum . fromNullable . map (\((_, v2) := _) -> v2)

operation_create_initial_indexes :: (MonadIO m) => OperationSpec m
operation_create_initial_indexes =
  ("ensure-base-indexes",) do
    needsAnalyze <- bookmarkStatsMissing
    pure
      $ (flip SqlStatement [] <$> indexStatements)
      -- on an already-analyzed database a fresh index has no sqlite_stat1 row,
      -- which can make the planner prefer it over the (time, id)-ordered
      -- indexes and force a sort step, so analyze once when stats are missing
      <> [SqlStatement "ANALYZE bookmark" [] | needsAnalyze]
  where
    bookmarkStatsMissing = do
      statTable <- rawSql "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'sqlite_stat1'" []
      case statTable :: [Single Int] of
        [] -> pure False
        _ -> do
          found <- rawSql "SELECT COUNT(*) FROM sqlite_stat1 WHERE tbl = 'bookmark' AND idx IN ('idx_bookmark_user_shared', 'idx_bookmark_id_shared')" []
          pure (found /= [Single (2 :: Int)])
    indexStatements =
      [ "CREATE INDEX IF NOT EXISTS idx_bookmark_time_id ON bookmark (user_id, time)",
        "CREATE INDEX IF NOT EXISTS idx_bookmark_shared_time_id ON bookmark (user_id, shared, time)",
        -- narrowest covering index for shared-filtered counts; without it the
        -- planner satisfies them from a wider expression index
        "CREATE INDEX IF NOT EXISTS idx_bookmark_user_shared ON bookmark (user_id, shared)",
        "CREATE INDEX IF NOT EXISTS idx_note_user_shared ON note (user_id, shared)",
        -- covers the shared check when joining bookmark_tag to bookmark by id
        -- (public tag cloud counts), avoiding row lookups in the main table
        "CREATE INDEX IF NOT EXISTS idx_bookmark_id_shared ON bookmark (id, shared)",
        "CREATE INDEX IF NOT EXISTS idx_bookmark_tag_bookmark_id ON bookmark_tag (bookmark_id, id, tag, seq)",
        "CREATE INDEX IF NOT EXISTS idx_note_created_id ON note (user_id, created)",
        "CREATE INDEX IF NOT EXISTS idx_note_shared_created_id ON note (user_id, shared, created)",
        -- expression must match Model.bookmarkHrefNoSchemeExpr token-for-token
        -- for SQLite to use these indexes when sorting by BookmarkSortUrl
        "CREATE INDEX IF NOT EXISTS idx_bookmark_href_no_scheme ON bookmark (user_id, (CASE WHEN instr(href, '://') > 0 THEN substr(href, instr(href, '://') + 3) ELSE href END))",
        "CREATE INDEX IF NOT EXISTS idx_bookmark_shared_href_no_scheme ON bookmark (user_id, shared, (CASE WHEN instr(href, '://') > 0 THEN substr(href, instr(href, '://') + 3) ELSE href END))",
        -- expression must match the `lower_ (b ^. BookmarkDescription)` used
        -- for BookmarkSortTitle in Model.bookmarksTagsQuery
        "CREATE INDEX IF NOT EXISTS idx_bookmark_title ON bookmark (user_id, (LOWER(description)))",
        "CREATE INDEX IF NOT EXISTS idx_bookmark_shared_title ON bookmark (user_id, shared, (LOWER(description)))",
        -- expression must match the `lower_ (b ^. NoteTitle)` used for
        -- NoteSortTitle in Model.getNoteList
        "CREATE INDEX IF NOT EXISTS idx_note_title ON note (user_id, (LOWER(title)))",
        "CREATE INDEX IF NOT EXISTS idx_note_shared_title ON note (user_id, shared, (LOWER(title)))"
      ]

-- superseded by the ASC (sort-field, id)-ordering indexes above
operation_drop_superseded_indexes :: (Applicative m) => OperationSpec m
operation_drop_superseded_indexes =
  ("drop-superseded-indexes",)
    $ pure
    $ flip SqlStatement []
    <$> [ "DROP INDEX IF EXISTS idx_bookmark_time",
          "DROP INDEX IF EXISTS idx_bookmark_shared_time",
          "DROP INDEX IF EXISTS idx_note_user_created",
          "DROP INDEX IF EXISTS idx_note_user_shared_created"
        ]

operation_normalize_bookmark_utctime_remove_z :: (Applicative m) => OperationSpec m
operation_normalize_bookmark_utctime_remove_z =
  ("normalize-bookmark-utctime-remove-z",)
    $ pure
    $ flip SqlStatement []
    <$> filter (not . T.null . T.strip) (T.splitOn ";" operationSql)
  where
    operationSql = decodeUtf8 $(embedFile "appmigrations/002-normalize-bookmark-utctime-remove-z.sql")

operation_normalize_note_utctime_remove_z :: (Applicative m) => OperationSpec m
operation_normalize_note_utctime_remove_z =
  ("normalize-note-utctime-remove-z",)
    $ pure
    $ flip SqlStatement []
    <$> filter (not . T.null . T.strip) (T.splitOn ";" operationSql)
  where
    operationSql = decodeUtf8 $(embedFile "appmigrations/002-normalize-note-utctime-remove-z.sql")

-- marker only: the unique_user_note_slug constraint itself is added by
-- runPersistentMigrations, after runPreMigrations has deduplicated any
-- conflicting note slugs
operation_marker_unique_user_note_slug :: (Applicative m) => OperationSpec m
operation_marker_unique_user_note_slug = ("marker-unique-user-note-slug", pure [])
