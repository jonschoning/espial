{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.NotesSearchSpec (spec) where

import Model.Custom (hashPasswordBCryptWithPolicy, mkNtSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User "testuser" pwHash Nothing False False True True False False True Nothing

createNote :: Key User -> Text -> Bool -> Bool -> DB (Key Note)
createNote uid title isMarkdown shared = do
  slug <- liftIO mkNtSlug
  insert $ Note uid slug (length title) title "text" isMarkdown shared t0 t0

search :: Key User -> SharedP -> Maybe Text -> DB [Key Note]
search uid sharedp mquery = do
  (_, rows, _, _) <- getNoteList uid mquery Nothing sharedp 100 1
  return $ map entityKey rows

spec :: Spec
spec = withApp $ do
  describe "getNoteList" $ do
    it "markdown:true matches only markdown notes" $ do
      (uid, nid1, nid2) <- runDB $ do
        uid <- createTestUser
        nid1 <- createNote uid "a" True False
        nid2 <- createNote uid "b" False False
        return (uid, nid1, nid2)
      nids <- runDB $ search uid SharedAll (Just "markdown:true")
      liftIO $ nids `shouldBe` [nid1]
      nids' <- runDB $ search uid SharedAll (Just "m:false")
      liftIO $ nids' `shouldBe` [nid2]

    it "falls back to all-field search for a non-boolean markdown value" $ do
      (uid, nid1) <- runDB $ do
        uid <- createTestUser
        nid1 <- createNote uid "markdown:tutorial" False False
        _ <- createNote uid "other" True False
        return (uid, nid1)
      nids <- runDB $ search uid SharedAll (Just "markdown:tutorial")
      liftIO $ nids `shouldBe` [nid1]

    it "the shared filter restricts results" $ do
      (uid, nid1, nid2) <- runDB $ do
        uid <- createTestUser
        nid1 <- createNote uid "a" False True
        nid2 <- createNote uid "b" False False
        return (uid, nid1, nid2)
      publicNids <- runDB $ search uid SharedPublic Nothing
      liftIO $ publicNids `shouldBe` [nid1]
      privateNids <- runDB $ search uid SharedPrivate Nothing
      liftIO $ privateNids `shouldBe` [nid2]
      allNids <- runDB $ search uid SharedAll Nothing
      liftIO $ sort allNids `shouldBe` sort [nid1, nid2]
