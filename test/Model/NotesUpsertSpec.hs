{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.NotesUpsertSpec (spec) where

import Model.Custom (NtSlug, hashPasswordBCryptWithPolicy, mkNtSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

t1 :: UTCTime
t1 = UTCTime (fromGregorian 2024 1 2) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User "testuser" pwHash Nothing False False True True False False True Nothing

createNoteAt :: Key User -> Text -> UTCTime -> DB (Key Note)
createNoteAt uid title updated = do
  slug <- liftIO mkNtSlug
  insert $ Note uid slug (length title) title "text" False False t0 updated

getNote' :: Key Note -> DB (Maybe Note)
getNote' nid = fmap entityVal <$> selectFirst [NoteId ==. nid] []

mkNoteEdit :: Key User -> NtSlug -> Text -> UTCTime -> Note
mkNoteEdit uid slug title clientKnownUpdated =
  Note
    { noteUserId = uid,
      noteSlug = slug,
      noteLength = length title,
      noteTitle = title,
      noteText = "edited text",
      noteIsMarkdown = False,
      noteShared = False,
      noteCreated = t0,
      noteUpdated = clientKnownUpdated
    }

spec :: Spec
spec = withApp $ do
  describe "upsertNote" $ do
    it "sets updated to now on a successful edit" $ do
      beforeTime <- liftIO getCurrentTime
      (uid, nid) <- runDB $ do
        uid <- createTestUser
        nid <- createNoteAt uid "a" t0
        return (uid, nid)
      slug <- liftIO mkNtSlug
      let edit = mkNoteEdit uid slug "edited" t0
      result <- runDB $ upsertNote uid (Just nid) edit
      afterTime <- liftIO getCurrentTime
      liftIO $ result `shouldBe` Updated nid
      mnote <- runDB $ getNote' nid
      case mnote of
        Nothing -> liftIO $ expectationFailure "note missing after update"
        Just note -> liftIO $ do
          noteUpdated note >= beforeTime `shouldBe` True
          noteUpdated note <= afterTime `shouldBe` True

    it "fails when the stored copy was updated more recently than the client's copy" $ do
      (uid, nid) <- runDB $ do
        uid <- createTestUser
        nid <- createNoteAt uid "a" t1
        return (uid, nid)
      slug <- liftIO mkNtSlug
      let edit = mkNoteEdit uid slug "edited" t0
      result <- runDB $ upsertNote uid (Just nid) edit
      liftIO $ result `shouldBe` Failed ReasonConflictWithNewer
      mnote <- runDB $ getNote' nid
      liftIO $ fmap noteTitle mnote `shouldBe` Just "a"

    it "creates a new note when no id is given" $ do
      uid <- runDB createTestUser
      slug <- liftIO mkNtSlug
      let note = mkNoteEdit uid slug "new" t0
      result <- runDB $ upsertNote uid Nothing note
      liftIO $ case result of
        Created _ -> pure ()
        other -> expectationFailure ("expected Created, got " <> show other)
