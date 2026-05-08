{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import ClassyPrelude
import qualified Data.Text as T
import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P
import Lens.Micro
import Model
import Model.Custom
import Model.File
import Model.FileNetscape (exportNetscapeBookmarks)
import qualified Options.Applicative as OA
import Options.Applicative.Help (suggestionsHelp)
import Options.Generic
import Settings (AppSettings (..))
import Types
import Yesod.Default.Config2 (configSettingsYml, loadYamlSettings, useEnv)

data MigrationOpts
  = CreateDB {conn :: Maybe Text}
  | CreateUser
      { conn :: Maybe Text,
        userName :: Text,
        userPassword :: Password,
        privateDefault :: Maybe Bool,
        archiveDefault :: Maybe Bool,
        suggestTags :: Maybe Bool,
        privacyLock :: Maybe Bool
      }
  | CreateApiKey
      { conn :: Maybe Text,
        userName :: Text
      }
  | DeleteUser
      { conn :: Maybe Text,
        userName :: Text
      }
  | ShowUser
      { conn :: Maybe Text,
        userName :: Text
      }
  | DeleteApiKey
      { conn :: Maybe Text,
        userName :: Text
      }
  | ImportBookmarks
      { conn :: Maybe Text,
        userName :: Text,
        bookmarkFile :: FilePath
      }
  | ExportBookmarks
      { conn :: Maybe Text,
        userName :: Text,
        bookmarkFile :: FilePath
      }
  | ImportFirefoxBookmarks
      { conn :: Maybe Text,
        userName :: Text,
        bookmarkFile :: FilePath
      }
  | ImportNetscapeBookmarks
      { conn :: Maybe Text,
        userName :: Text,
        bookmarkFile :: FilePath
      }
  | ExportNetscapeBookmarks
      { conn :: Maybe Text,
        userName :: Text,
        bookmarkFile :: FilePath
      }
  | ImportNotes
      { conn :: Maybe Text,
        userName :: Text,
        noteDirectory :: FilePath
      }
  | PrintMigrateDB {conn :: Maybe Text}
  deriving (Generic, Show)

instance ParseRecord MigrationOpts

main :: IO ()
main = do
  args <- getRecord "Migrations"
  case args of
    PrintMigrateDB {..} -> do
      connText <- getConnText conn
      P.runSqlite connText dumpMigration
    CreateDB {..} -> do
      connText <- getConnText conn
      let connInfo =
            P.mkSqliteConnectionInfo connText
              & set P.fkEnabled False
      P.runSqliteInfo connInfo runMigrations
    CreateUser {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        passwordText <- liftIO . fmap T.strip $ case userPassword of
          PasswordText s -> pure s
          PasswordFile f -> readFileUtf8 f
        hash' <- liftIO (hashPassword passwordText)
        void $
          P.upsertBy
            (UniqueUserName userName)
            (User userName hash' Nothing False False True False)
            [ UserPasswordHash P.=. hash',
              UserPrivateDefault P.=. fromMaybe False privateDefault,
              UserArchiveDefault P.=. fromMaybe False archiveDefault,
              UserSuggestTags P.=. fromMaybe True suggestTags,
              UserPrivacyLock P.=. fromMaybe False privacyLock
            ]
        pure () :: DB ()
    ShowUser {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Nothing -> liftIO (print (userName ++ " not found"))
          Just (P.Entity _ user) -> liftIO (putStrLn (formatUserSummary user))
    CreateApiKey {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        apiKey@(ApiKey plainKey) <- liftIO generateApiKey
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Nothing -> liftIO (print (userName ++ " not found"))
          Just (P.Entity uid _) -> do
            -- API key is only displayed once after creation,
            -- since it is stored in hashed form.
            let hashedKey = hashApiKey apiKey
            P.update uid [UserApiToken P.=. Just hashedKey]
            liftIO $ print plainKey
    DeleteApiKey {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Nothing -> liftIO (print (userName ++ " not found"))
          Just (P.Entity uid _) -> do
            P.update uid [UserApiToken P.=. Nothing]
    DeleteUser {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Nothing -> liftIO (print (userName ++ "not found"))
          Just (P.Entity uid _) -> do
            P.delete uid
            pure () :: DB ()
    ExportBookmarks {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> exportFileBookmarks uid bookmarkFile
          Nothing -> liftIO (print (userName ++ "not found"))
    ImportBookmarks {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> do
            result <- insertFileBookmarks uid bookmarkFile
            case result of
              Left e -> liftIO (print e)
              Right n -> liftIO (print (show n ++ " bookmarks imported."))
          Nothing -> liftIO (print (userName ++ "not found"))
    ImportFirefoxBookmarks {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> do
            result <- insertFirefoxBookmarks uid bookmarkFile
            case result of
              Left e -> liftIO (print e)
              Right n -> liftIO (print (show n ++ " bookmarks imported."))
          Nothing -> liftIO (print (userName ++ "not found"))
    ImportNotes {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> do
            result <- insertDirFileNotes uid noteDirectory
            case result of
              Left e -> liftIO (print e)
              Right n -> liftIO (print (show n ++ " notes imported."))
          Nothing -> liftIO (print (userName ++ "not found"))
    ImportNetscapeBookmarks {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> do
            result <- insertNetscapeBookmarks uid bookmarkFile
            case result of
              Left e -> liftIO (print e)
              Right n -> liftIO (print (show n ++ " bookmarks imported."))
          Nothing -> liftIO (print (userName ++ "not found"))
    ExportNetscapeBookmarks {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> exportNetscapeBookmarks uid bookmarkFile
          Nothing -> liftIO (print (userName ++ "not found"))
  where
    getConnText :: Maybe Text -> IO Text
    getConnText mconn =
      maybe
        (P.sqlDatabase . appDatabaseConf <$> loadYamlSettings [configSettingsYml] [] useEnv)
        pure
        mconn

    formatUserSummary :: User -> Text
    formatUserSummary User {..} =
      intercalate
        "\n"
        [ "name: " <> userName,
          "privateDefault: " <> tshow userPrivateDefault,
          "archiveDefault: " <> tshow userArchiveDefault,
          "suggestTags: " <> tshow userSuggestTags,
          "privacyLock: " <> tshow userPrivacyLock,
          "hasApiKey: " <> tshow (isJust userApiToken)
        ]

data Password
  = PasswordText Text
  | PasswordFile FilePath
  deriving (Show, Read)

parsePassword :: OA.Parser Password
parsePassword = passwordText <|> passwordFile
  where
    passwordText =
      PasswordText
        <$> OA.strOption
          ( OA.long "userPassword"
              <> OA.metavar "PASSWORD"
              <> OA.help "Password in plain-text"
          )

    passwordFile =
      PasswordFile
        <$> OA.strOption
          ( OA.long "userPasswordFile"
              <> OA.metavar "FILE"
              <> OA.help "Password file"
          )

instance ParseFields Password

instance ParseRecord Password where
  parseRecord = fmap getOnly parseRecord

instance ParseField Password where
  parseField _ _ _ _ = parsePassword
