{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.BookmarksBulkEditSpec (spec) where

import Database.Persist.Sql (fromSqlKey)
import Model.Custom (hashPasswordBCryptWithPolicy, mkBmSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User "testuser" pwHash Nothing False False True True False False True Nothing

createBm :: Key User -> Text -> DB (Key Bookmark)
createBm uid href = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href "desc" "" t0 False False False Nothing

createUnreadBm :: Key User -> Text -> DB (Key Bookmark)
createUnreadBm uid href = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href "desc" "" t0 False True False Nothing

tagBm :: Key User -> Key Bookmark -> Text -> Int -> DB ()
tagBm uid bid tag seq' = insert_ $ BookmarkTag uid tag bid seq'

getBookmarkTags :: Key Bookmark -> DB [Text]
getBookmarkTags bid = do
  tags <- selectList [BookmarkTagBookmarkId ==. bid] [Asc BookmarkTagSeq]
  return $ map (bookmarkTagTag . entityVal) tags

mkBulkEditForm :: Key User -> Int -> [Text] -> FilterP -> Text -> Text -> Maybe BulkAction -> [Key Bookmark] -> BulkEditForm
mkBulkEditForm _uid cnt filterTags filterP addTags removeTags action bids =
  BulkEditForm
    { _beSelection =
        if null bids
          then BulkSelectionAll filterP SharedAll filterTags Nothing
          else BulkSelectionPage (map fromSqlKey bids),
      _beAction = action,
      _beAddTags = addTags,
      _beRemoveTags = removeTags,
      _beSelectionCount = cnt
    }

spec :: Spec
spec = withApp $ do
  describe "bookmarksBulkEdit" $ do
    describe "BulkSelectionAll - tag stability" $ do
      it "remove+add same filter tag re-adds to the original bookmark set" $ do
        (uid, bid1, bid2) <- runDB $ do
          uid <- createTestUser
          bid1 <- createBm uid "https://a.com"
          bid2 <- createBm uid "https://b.com"
          tagBm uid bid1 "tag1" 1
          tagBm uid bid2 "tag1" 1
          return (uid, bid1, bid2)
        let form = mkBulkEditForm uid 2 ["tag1"] FilterAll "tag1" "tag1" Nothing []
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Right 2
        tags1 <- runDB $ getBookmarkTags bid1
        tags2 <- runDB $ getBookmarkTags bid2
        liftIO $ tags1 `shouldBe` ["tag1"]
        liftIO $ tags2 `shouldBe` ["tag1"]

      it "remove filter tag and add different tag operates on the original set" $ do
        (uid, bid1, bid2) <- runDB $ do
          uid <- createTestUser
          bid1 <- createBm uid "https://a.com"
          bid2 <- createBm uid "https://b.com"
          tagBm uid bid1 "tag1" 1
          tagBm uid bid2 "tag1" 1
          return (uid, bid1, bid2)
        let form = mkBulkEditForm uid 2 ["tag1"] FilterAll "tag2" "tag1" Nothing []
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Right 2
        tags1 <- runDB $ getBookmarkTags bid1
        tags2 <- runDB $ getBookmarkTags bid2
        liftIO $ tags1 `shouldBe` ["tag2"]
        liftIO $ tags2 `shouldBe` ["tag2"]

      it "action that changes the filter does not shrink the tag operation set" $ do
        (uid, bid1, bid2) <- runDB $ do
          uid <- createTestUser
          bid1 <- createUnreadBm uid "https://a.com"
          bid2 <- createUnreadBm uid "https://b.com"
          return (uid, bid1, bid2)
        let form = mkBulkEditForm uid 2 [] FilterUnread "tag1" "" (Just BulkActionRead) []
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Right 2
        mBm1 <- runDB $ selectFirst [BookmarkId ==. bid1] []
        mBm2 <- runDB $ selectFirst [BookmarkId ==. bid2] []
        liftIO $ fmap (bookmarkToRead . entityVal) mBm1 `shouldBe` Just False
        liftIO $ fmap (bookmarkToRead . entityVal) mBm2 `shouldBe` Just False
        tags1 <- runDB $ getBookmarkTags bid1
        tags2 <- runDB $ getBookmarkTags bid2
        liftIO $ tags1 `shouldBe` ["tag1"]
        liftIO $ tags2 `shouldBe` ["tag1"]

      it "add only does not duplicate an already-present tag" $ do
        (uid, bid1) <- runDB $ do
          uid <- createTestUser
          bid1 <- createBm uid "https://a.com"
          tagBm uid bid1 "tag1" 1
          return (uid, bid1)
        let form = mkBulkEditForm uid 1 [] FilterAll "tag1 tag2" "" Nothing []
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Right 1
        tags <- runDB $ getBookmarkTags bid1
        liftIO $ tags `shouldBe` ["tag1", "tag2"]

      it "remove treats _ and % in tag names as literals, not SQL wildcards" $ do
        (uid, bid1, bid2) <- runDB $ do
          uid <- createTestUser
          bid1 <- createBm uid "https://a.com"
          bid2 <- createBm uid "https://b.com"
          tagBm uid bid1 "foo_bar" 1
          tagBm uid bid1 "fooXbar" 2
          tagBm uid bid2 "fooXbar" 1
          return (uid, bid1, bid2)
        let form = mkBulkEditForm uid 2 [] FilterAll "" "foo_bar" Nothing []
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Right 2
        tags1 <- runDB $ getBookmarkTags bid1
        tags2 <- runDB $ getBookmarkTags bid2
        liftIO $ tags1 `shouldBe` ["fooXbar"]
        liftIO $ tags2 `shouldBe` ["fooXbar"]

      it "remove is case-insensitive when stored tag casing differs from input" $ do
        (uid, bid1) <- runDB $ do
          uid <- createTestUser
          bid1 <- createBm uid "https://a.com"
          tagBm uid bid1 "Tag1" 1
          return (uid, bid1)
        let form = mkBulkEditForm uid 1 [] FilterAll "" "tag1" Nothing []
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Right 1
        tags <- runDB $ getBookmarkTags bid1
        liftIO $ tags `shouldBe` []

    describe "BulkSelectionPage - tag stability" $ do
      it "remove+add same tag via page selection re-adds correctly" $ do
        (uid, bid1, bid2) <- runDB $ do
          uid <- createTestUser
          bid1 <- createBm uid "https://a.com"
          bid2 <- createBm uid "https://b.com"
          tagBm uid bid1 "tag1" 1
          tagBm uid bid2 "tag1" 1
          return (uid, bid1, bid2)
        let form = mkBulkEditForm uid 2 ["tag1"] FilterAll "tag1" "tag1" Nothing [bid1, bid2]
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Right 2
        tags1 <- runDB $ getBookmarkTags bid1
        tags2 <- runDB $ getBookmarkTags bid2
        liftIO $ tags1 `shouldBe` ["tag1"]
        liftIO $ tags2 `shouldBe` ["tag1"]

      it "page count mismatch returns Left BulkEditErrorPageMismatch" $ do
        (uid, bid1, bid2) <- runDB $ do
          uid <- createTestUser
          bid1 <- createBm uid "https://a.com"
          bid2 <- createBm uid "https://b.com"
          return (uid, bid1, bid2)
        let form = mkBulkEditForm uid 1 [] FilterAll "" "" Nothing [bid1, bid2]
        result <- runDB $ bookmarksBulkEdit uid form
        liftIO $ result `shouldBe` Left BulkEditErrorPageMismatch
