{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import ClassyPrelude
import Data.Text qualified as T
import Database.Persist qualified as P
import Database.Persist.Sqlite qualified as P
import Import (configSettingsYmlValue)
import Lens.Micro
import Model
import Model.Custom
import Model.File
import Model.Migrations (dumpMigration, runAppMigrations, runPersistentMigrations, runPreMigrations)
import Options.Applicative qualified as OA
import Options.Generic
import Settings (AppSettings (..), appPasswordHashConfig)
import Types
import Web.ClientSession qualified as CS
import Yesod.Default.Config2 (configSettingsYml, loadYamlSettings, useEnv)

data MigrationOpts
  = CreateDB
      { conn :: Maybe Text,
        silent :: Maybe Bool
      }
  | CreateUser
      { conn :: Maybe Text,
        userName :: Text,
        userPassword :: Password,
        privateDefault :: Maybe Bool,
        archiveDefault :: Maybe Bool,
        suggestTags :: Maybe Bool,
        suggestTagsUseReturnKey :: Maybe Bool,
        privacyLock :: Maybe Bool,
        publicTagCloud :: Maybe Bool,
        previewNotes :: Maybe Bool,
        userLanguage :: Maybe Language
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
  | ImportNotesJson
      { conn :: Maybe Text,
        userName :: Text,
        noteFile :: FilePath
      }
  | ExportNotesJson
      { conn :: Maybe Text,
        userName :: Text,
        noteFile :: FilePath
      }
  | PrintMigrateDB {conn :: Maybe Text}
  | RunMigrateDB
      { conn :: Maybe Text,
        silent :: Maybe Bool
      }
  | GenerateSessionKey
  deriving (Generic, Show)

instance ParseRecord MigrationOpts

parseI18nLang :: OA.ReadM I18nLang
parseI18nLang =
  OA.str >>= \s ->
    maybe
      (OA.readerError $ "Invalid language: '" <> s <> "' Supported languages: " <> supportedLangs)
      pure
      (toI18nLang s)

newtype Language = Language {unLanguage :: I18nLang}
  deriving (Show)

instance ParseField Language where
  readField = Language <$> parseI18nLang
  metavar _ = "LANGUAGE"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getRecord "Migrations"
  case args of
    PrintMigrateDB {..} -> do
      connText <- getConnText conn
      P.runSqlite connText dumpMigration
    RunMigrateDB {..} -> do
      connText <- getConnText conn
      let connInfo =
            P.mkSqliteConnectionInfo connText
              & set P.fkEnabled False
      P.runSqliteInfo connInfo (runPreMigrations $ maybe True not silent)
      P.runSqliteInfo connInfo (runPersistentMigrations $ maybe True not silent)
      P.runSqliteInfo connInfo (runAppMigrations $ maybe True not silent)
    CreateDB {..} -> do
      connText <- getConnText conn
      let connInfo =
            P.mkSqliteConnectionInfo connText
              & set P.fkEnabled False
      P.runSqliteInfo connInfo (runPreMigrations $ maybe True not silent)
      P.runSqliteInfo connInfo (runPersistentMigrations $ maybe True not silent)
      P.runSqliteInfo connInfo (runAppMigrations $ maybe True not silent)
    CreateUser {..} -> do
      connText <- getConnText conn
      settings <- loadYamlSettings [configSettingsYml] [configSettingsYmlValue] useEnv
      P.runSqlite connText $ do
        passwordText <- liftIO . fmap T.strip $ case userPassword of
          PasswordText s -> pure s
          PasswordFile f -> readFileUtf8 f
        hash' <- liftIO (hashPasswordWith (appPasswordHashConfig settings) passwordText)
        let privateDefaultVal = fromMaybe False privateDefault
            archiveDefaultVal = fromMaybe False archiveDefault
            suggestTagsVal = fromMaybe True suggestTags
            suggestTagsUseReturnKeyVal = fromMaybe True suggestTagsUseReturnKey
            privacyLockVal = fromMaybe False privacyLock
            publicTagCloudVal = fromMaybe False publicTagCloud
            previewNotesVal = fromMaybe True previewNotes
            userLanguageVal = unLanguage <$> userLanguage
        void $
          P.upsertBy
            (UniqueUserName userName)
            (User userName hash' Nothing privateDefaultVal archiveDefaultVal suggestTagsVal suggestTagsUseReturnKeyVal privacyLockVal publicTagCloudVal previewNotesVal userLanguageVal)
            [ UserPasswordHash P.=. hash',
              UserPrivateDefault P.=. privateDefaultVal,
              UserArchiveDefault P.=. archiveDefaultVal,
              UserSuggestTags P.=. suggestTagsVal,
              UserSuggestTagsUseReturnKey P.=. suggestTagsUseReturnKeyVal,
              UserPrivacyLock P.=. privacyLockVal,
              UserPublicTagCloud P.=. publicTagCloudVal,
              UserPreviewNotes P.=. previewNotesVal,
              UserLanguage P.=. userLanguageVal
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
      mfmarks <- readFileBookmarks bookmarkFile
      case mfmarks of
        Left e -> print e
        Right fmarks ->
          P.runSqlite connText $ do
            muser <- P.getBy (UniqueUserName userName)
            case muser of
              Just (P.Entity uid _) -> do
                n <- insertFileBookmarks uid fmarks
                liftIO (print (show n ++ " bookmarks imported."))
              Nothing -> liftIO (print (userName ++ "not found"))
    ImportFirefoxBookmarks {..} -> do
      connText <- getConnText conn
      mfmarks <- readFirefoxBookmarks bookmarkFile
      case mfmarks of
        Left e -> print e
        Right fmarks ->
          P.runSqlite connText $ do
            muser <- P.getBy (UniqueUserName userName)
            case muser of
              Just (P.Entity uid _) -> do
                n <- insertFirefoxBookmarks uid fmarks
                liftIO (print (show n ++ " bookmarks imported."))
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
    ImportNotesJson {..} -> do
      connText <- getConnText conn
      mfnotes <- readFileNotes noteFile
      case mfnotes of
        Left e -> print e
        Right fnotes ->
          P.runSqlite connText $ do
            muser <- P.getBy (UniqueUserName userName)
            case muser of
              Just (P.Entity uid _) -> do
                n <- insertFileNotes uid fnotes
                liftIO (print (show n ++ " notes imported."))
              Nothing -> liftIO (print (userName ++ "not found"))
    ExportNotesJson {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> exportFileNotes uid noteFile
          Nothing -> liftIO (print (userName ++ "not found"))
    ImportNetscapeBookmarks {..} -> do
      connText <- getConnText conn
      mcontent <- tryAny (readFile bookmarkFile)
      case mcontent of
        Left _ -> print ("Could not read file" :: Text)
        Right (bs :: ByteString) ->
          case parseNetscapeBookmarks (decodeUtf8 bs) of
            Left e -> print e
            Right nbmarks ->
              P.runSqlite connText $ do
                muser <- P.getBy (UniqueUserName userName)
                case muser of
                  Just (P.Entity uid _) -> do
                    n <- insertNetscapeBookmarks uid nbmarks
                    liftIO (print (show n ++ " bookmarks imported."))
                  Nothing -> liftIO (print (userName ++ "not found"))
    ExportNetscapeBookmarks {..} -> do
      connText <- getConnText conn
      P.runSqlite connText $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> exportNetscapeBookmarks uid bookmarkFile
          Nothing -> liftIO (print (userName ++ "not found"))
    GenerateSessionKey -> void $ CS.randomKeyEnv "CLIENT_SESSION_KEY"
  where
    getConnText :: Maybe Text -> IO Text
    getConnText mconn =
      maybe
        (P.sqlDatabase . appDatabaseConf <$> loadYamlSettings [configSettingsYml] [configSettingsYmlValue] useEnv)
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
          "suggestTagsUseReturnKey: " <> tshow userSuggestTagsUseReturnKey,
          "privacyLock: " <> tshow userPrivacyLock,
          "publicTagCloud: " <> tshow userPublicTagCloud,
          "previewNotes: " <> tshow userPreviewNotes,
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
