{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Types
import Model
import ModelCrypto
import Pretty

import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P

import ClassyPrelude

import Options.Generic

data MigrationOpts
  = CreateDB { conn :: Text}
  | CreateUser { conn :: Text
               , userName :: Text
               , userPassword :: Text
               , userApiToken :: Maybe Text}
  | DeleteUser { conn :: Text
               , userName :: Text
               }
  | ImportBookmarks { conn :: Text
                    , userName :: Text
                    , bookmarkFile :: FilePath}
  deriving (Generic, Show)

instance ParseRecord MigrationOpts

main :: IO ()
main =
  getRecord "Migrations" >>=
  \case
    CreateDB conn -> P.runSqlite conn runMigrations
    CreateUser conn uname upass utoken ->
      P.runSqlite conn $
      do hash' <- liftIO $ hashPassword upass
         user <-
           P.upsertBy
             (UniqueUserName uname)
             (User uname hash' utoken)
             [UserPasswordHash P.=. hash', UserApiToken P.=. utoken]
         liftIO $ cpprint user
         pure () :: DB ()
    DeleteUser conn uname ->
      P.runSqlite conn $
      do P.getBy (UniqueUserName uname) >>=
           \case
             Just (P.Entity uid _) -> do
               P.deleteCascade uid
               pure () :: DB ()
             Nothing -> liftIO $ print $ uname ++ "not found"
    ImportBookmarks conn uname file ->
      P.runSqlite conn $
      do P.getBy (UniqueUserName uname) >>=
           \case
             Just (P.Entity uid _) -> insertFileBookmarks uid file
             Nothing -> liftIO $ print $ uname ++ "not found"
