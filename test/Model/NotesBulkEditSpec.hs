{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.NotesBulkEditSpec (spec) where

import Database.Persist.Sql (fromSqlKey)
import Model.Custom (hashPasswordBCryptWithPolicy, mkNtSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User "testuser" pwHash Nothing False False True True False False True Nothing

createNote :: Key User -> Text -> Bool -> DB (Key Note)
createNote uid title = createNoteIsMarkdown uid title False

createNoteIsMarkdown :: Key User -> Text -> Bool -> Bool -> DB (Key Note)
createNoteIsMarkdown uid title isMarkdown shared = do
  slug <- liftIO mkNtSlug
  insert $ Note uid slug (length title) title "text" isMarkdown shared t0 t0

getNoteShared :: Key Note -> DB (Maybe Bool)
getNoteShared nid = fmap (fmap (noteShared . entityVal)) (selectFirst [NoteId ==. nid] [])

getNoteIsMarkdown :: Key Note -> DB (Maybe Bool)
getNoteIsMarkdown nid = fmap (fmap (noteIsMarkdown . entityVal)) (selectFirst [NoteId ==. nid] [])

getNoteExists :: Key Note -> DB Bool
getNoteExists nid = fmap isJust (selectFirst [NoteId ==. nid] [])

mkNoteBulkEditForm :: Int -> Maybe Text -> NoteBulkAction -> [Key Note] -> NoteBulkEditForm
mkNoteBulkEditForm cnt mquery action nids =
  NoteBulkEditForm
    { _nbeSelection =
        if null nids
          then NoteBulkSelectionAll mquery
          else NoteBulkSelectionPage (map fromSqlKey nids),
      _nbeAction = action,
      _nbeSelectionCount = cnt
    }

spec :: Spec
spec = withApp $ do
  describe "notesBulkEdit" $ do
    describe "NoteBulkSelectionPage" $ do
      it "makes selected notes private" $ do
        (uid, nid1, nid2) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNote uid "a" True
          nid2 <- createNote uid "b" True
          return (uid, nid1, nid2)
        let form = mkNoteBulkEditForm 2 Nothing NoteBulkActionPrivate [nid1, nid2]
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Right 2
        shared1 <- runDB $ getNoteShared nid1
        shared2 <- runDB $ getNoteShared nid2
        liftIO $ shared1 `shouldBe` Just False
        liftIO $ shared2 `shouldBe` Just False

      it "makes selected notes public" $ do
        (uid, nid1) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNote uid "a" False
          return (uid, nid1)
        let form = mkNoteBulkEditForm 1 Nothing NoteBulkActionPublic [nid1]
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Right 1
        shared1 <- runDB $ getNoteShared nid1
        liftIO $ shared1 `shouldBe` Just True

      it "sets selected notes to use markdown" $ do
        (uid, nid1, nid2) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNoteIsMarkdown uid "a" False False
          nid2 <- createNoteIsMarkdown uid "b" False False
          return (uid, nid1, nid2)
        let form = mkNoteBulkEditForm 1 Nothing NoteBulkActionMarkdown [nid1]
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Right 1
        md1 <- runDB $ getNoteIsMarkdown nid1
        md2 <- runDB $ getNoteIsMarkdown nid2
        liftIO $ md1 `shouldBe` Just True
        liftIO $ md2 `shouldBe` Just False

      it "sets selected notes to use plaintext" $ do
        (uid, nid1) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNoteIsMarkdown uid "a" True False
          return (uid, nid1)
        let form = mkNoteBulkEditForm 1 Nothing NoteBulkActionPlaintext [nid1]
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Right 1
        md1 <- runDB $ getNoteIsMarkdown nid1
        liftIO $ md1 `shouldBe` Just False

      it "deletes only the selected notes" $ do
        (uid, nid1, nid2) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNote uid "a" False
          nid2 <- createNote uid "b" False
          return (uid, nid1, nid2)
        let form = mkNoteBulkEditForm 1 Nothing NoteBulkActionDelete [nid1]
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Right 1
        exists1 <- runDB $ getNoteExists nid1
        exists2 <- runDB $ getNoteExists nid2
        liftIO $ exists1 `shouldBe` False
        liftIO $ exists2 `shouldBe` True

      it "does not modify notes of another user" $ do
        (uid1, nid2) <- runDB $ do
          uid1 <- createTestUser
          pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
          uid2 <- insert $ User "otheruser" pwHash Nothing False False True True False False True Nothing
          nid2 <- createNote uid2 "other" True
          return (uid1, nid2)
        let form = mkNoteBulkEditForm 1 Nothing NoteBulkActionPrivate [nid2]
        _ <- runDB $ notesBulkEdit uid1 form
        shared2 <- runDB $ getNoteShared nid2
        liftIO $ shared2 `shouldBe` Just True

      it "page count mismatch returns Left BulkEditErrorPageMismatch" $ do
        (uid, nid1, nid2) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNote uid "a" False
          nid2 <- createNote uid "b" False
          return (uid, nid1, nid2)
        let form = mkNoteBulkEditForm 1 Nothing NoteBulkActionDelete [nid1, nid2]
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Left BulkEditErrorPageMismatch

    describe "NoteBulkSelectionAll" $ do
      it "applies the action to all notes matching the query" $ do
        (uid, nid1, nid2) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNote uid "groceries" True
          nid2 <- createNote uid "work" True
          return (uid, nid1, nid2)
        let form = mkNoteBulkEditForm 1 (Just "groceries") NoteBulkActionPrivate []
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Right 1
        shared1 <- runDB $ getNoteShared nid1
        shared2 <- runDB $ getNoteShared nid2
        liftIO $ shared1 `shouldBe` Just False
        liftIO $ shared2 `shouldBe` Just True

      it "deletes all notes when no query is given" $ do
        (uid, nid1, nid2) <- runDB $ do
          uid <- createTestUser
          nid1 <- createNote uid "a" False
          nid2 <- createNote uid "b" True
          return (uid, nid1, nid2)
        let form = mkNoteBulkEditForm 2 Nothing NoteBulkActionDelete []
        result <- runDB $ notesBulkEdit uid form
        liftIO $ result `shouldBe` Right 2
        exists1 <- runDB $ getNoteExists nid1
        exists2 <- runDB $ getNoteExists nid2
        liftIO $ exists1 `shouldBe` False
        liftIO $ exists2 `shouldBe` False
