{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Types
import Model
import ModelCustom

import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P
import ClassyPrelude
import Lens.Micro

import Options.Generic
import qualified Options.Applicative as OA
import qualified Data.Text as T

data Password
  = PasswordText Text
  | PasswordFile FilePath
  deriving (Show, Read)

parsePassword :: OA.Parser Password
parsePassword = passwordText <|> passwordFile
  where
    passwordText = PasswordText <$> OA.strOption
      ( OA.long "userPassword"
          <> OA.metavar "PASSWORD"
          <> OA.help "Password in plain-text"
      )

    passwordFile = PasswordFile <$> OA.strOption
      ( OA.long "userPasswordFile"
          <> OA.metavar "FILE"
          <> OA.help "Password file"
      )

instance ParseFields Password

instance ParseRecord Password where
  parseRecord = fmap getOnly parseRecord

instance ParseField Password where
  parseField _ _ _ _ = parsePassword

data MigrationOpts
  = CreateDB { conn :: Text }
  | CreateUser { conn :: Text
               , userName :: Text
               , userPassword :: Password
               , privateDefault :: Maybe Bool
               , archiveDefault :: Maybe Bool
               , privacyLock :: Maybe Bool }
  | DeleteUser { conn :: Text
               , userName :: Text }
  | ImportBookmarks { conn :: Text
                    , userName :: Text
                    , bookmarkFile :: FilePath }
  | ImportFirefoxBookmarks { conn :: Text
                    , userName :: Text
                    , bookmarkFile :: FilePath }
  | ExportBookmarks { conn :: Text
                    , userName :: Text
                    , bookmarkFile :: FilePath }
  | ImportNotes { conn :: Text
                , userName :: Text
                , noteDirectory :: FilePath }
  | PrintMigrateDB { conn :: Text }
  deriving (Generic, Show)

instance ParseRecord MigrationOpts

main :: IO ()
main = do
  args <- getRecord "Migrations"
  case args of
    PrintMigrateDB {..} ->
      P.runSqlite conn dumpMigration

    CreateDB {..} -> do
      let connInfo = P.mkSqliteConnectionInfo conn
                     & set P.fkEnabled False
      P.runSqliteInfo connInfo runMigrations

    CreateUser{..} ->
      P.runSqlite conn $ do
        passwordText <- liftIO . fmap T.strip $ case userPassword of
          PasswordText s -> pure s
          PasswordFile f -> readFileUtf8 f
        hash' <- liftIO (hashPassword passwordText)
        void $ P.upsertBy
          (UniqueUserName userName)
          (User userName hash' Nothing False False False)
          [ UserPasswordHash P.=. hash'
          , UserApiToken P.=. Nothing
          , UserPrivateDefault P.=. fromMaybe False privateDefault
          , UserArchiveDefault P.=. fromMaybe False archiveDefault
          , UserPrivacyLock P.=. fromMaybe False privacyLock
          ]
        pure () :: DB ()

    DeleteUser {..} ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Nothing -> liftIO (print (userName ++ "not found"))
          Just (P.Entity uid _) -> do
            P.delete uid
            pure () :: DB ()

    ExportBookmarks {..} ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> exportFileBookmarks uid bookmarkFile
          Nothing -> liftIO (print (userName ++ "not found"))

    ImportBookmarks {..} ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> do
            result <- insertFileBookmarks uid bookmarkFile
            case result of
              Left e -> liftIO (print e)
              Right n -> liftIO (print (show n ++ " bookmarks imported."))
          Nothing -> liftIO (print (userName ++ "not found"))


    ImportFirefoxBookmarks {..} ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> do
            result <- insertFFBookmarks uid bookmarkFile
            case result of
              Left e -> liftIO (print e)
              Right n -> liftIO (print (show n ++ " bookmarks imported."))
          Nothing -> liftIO (print (userName ++ "not found"))

    ImportNotes {..} ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName userName)
        case muser of
          Just (P.Entity uid _) -> do
            result <- insertDirFileNotes uid noteDirectory
            case result of
              Left e -> liftIO (print e)
              Right n -> liftIO (print (show n ++ " notes imported."))
          Nothing -> liftIO (print (userName ++ "not found"))
