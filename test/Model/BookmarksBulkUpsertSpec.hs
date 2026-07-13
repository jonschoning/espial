{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.BookmarksBulkUpsertSpec (spec) where

import Database.Persist.Sql (toSqlKey)
import Model.Custom (hashPasswordBCryptWithPolicy, mkBmSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

createTestUser :: Text -> DB (Key User)
createTestUser name = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User name pwHash Nothing False False True True False False True Nothing

createBm :: Key User -> Text -> Maybe Text -> DB (Key Bookmark)
createBm uid href marchive = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href "desc" "ext" t0 False False False marchive

mkNewBm :: Key User -> Text -> DB Bookmark
mkNewBm uid href = do
  slug <- liftIO mkBmSlug
  pure $ Bookmark uid slug href "new desc" "new ext" t0 False False False Nothing

getBookmark :: Key Bookmark -> DB (Maybe Bookmark)
getBookmark bid = fmap entityVal <$> selectFirst [BookmarkId ==. bid] []

getBookmarkTags :: Key Bookmark -> DB [Text]
getBookmarkTags bid = do
  tags <- selectList [BookmarkTagBookmarkId ==. bid] [Asc BookmarkTagSeq]
  return $ map (bookmarkTagTag . entityVal) tags

isCreated :: UpsertResult a -> Bool
isCreated (Created _) = True
isCreated _ = False

spec :: Spec
spec = withApp $ do
  describe "upsertBookmarks" $ do
    it "inserts new bookmarks and creates their tags" $ do
      (uid, bm1, bm2) <- runDB $ do
        uid <- createTestUser "testuser"
        bm1 <- mkNewBm uid "https://a.com"
        bm2 <- mkNewBm uid "https://b.com"
        return (uid, bm1, bm2)
      results <- runDB $ upsertBookmarks uid [Nothing, Nothing] [bm1, bm2] [["tag1"], ["tag2", "tag3"]]
      liftIO $ map isCreated results `shouldBe` [True, True]
      case [bid | Created bid <- results] of
        [bid1, bid2] -> do
          tags1 <- runDB $ getBookmarkTags bid1
          tags2 <- runDB $ getBookmarkTags bid2
          liftIO $ tags1 `shouldBe` ["tag1"]
          liftIO $ tags2 `shouldBe` ["tag2", "tag3"]
        other -> liftIO $ expectationFailure ("unexpected results: " <> show other)

    it "updates an existing bookmark found by explicit id and preserves its slug" $ do
      (uid, bid, prevSlug, bm') <- runDB $ do
        uid <- createTestUser "testuser"
        bid <- createBm uid "https://a.com" Nothing
        Just prev <- getBookmark bid
        bm' <- mkNewBm uid "https://a.com"
        return (uid, bid, bookmarkSlug prev, bm')
      results <- runDB $ upsertBookmarks uid [Just bid] [bm'] [["newtag"]]
      liftIO $ results `shouldBe` [Updated bid]
      mUpdated <- runDB $ getBookmark bid
      liftIO $ fmap bookmarkSlug mUpdated `shouldBe` Just prevSlug
      liftIO $ fmap bookmarkDescription mUpdated `shouldBe` Just "new desc"
      tags <- runDB $ getBookmarkTags bid
      liftIO $ tags `shouldBe` ["newtag"]

    it "updates an existing bookmark found by href when no id is given" $ do
      (uid, bid, bm') <- runDB $ do
        uid <- createTestUser "testuser"
        bid <- createBm uid "https://a.com" Nothing
        bm' <- mkNewBm uid "https://a.com"
        return (uid, bid, bm')
      results <- runDB $ upsertBookmarks uid [Nothing] [bm'] [["newtag"]]
      liftIO $ results `shouldBe` [Updated bid]
      mUpdated <- runDB $ getBookmark bid
      liftIO $ fmap bookmarkDescription mUpdated `shouldBe` Just "new desc"

    it "preserves archiveHref when href is unchanged, clears it when href changes" $ do
      (uid, bidSame, bidChanged, bmSame, bmChanged) <- runDB $ do
        uid <- createTestUser "testuser"
        bidSame <- createBm uid "https://same.com" (Just "https://archive.org/same")
        bidChanged <- createBm uid "https://old.com" (Just "https://archive.org/old")
        bmSame <- mkNewBm uid "https://same.com"
        bmChanged <- mkNewBm uid "https://new.com"
        return (uid, bidSame, bidChanged, bmSame, bmChanged)
      _ <- runDB $ upsertBookmarks uid [Just bidSame, Just bidChanged] [bmSame, bmChanged] [[], []]
      mSame <- runDB $ getBookmark bidSame
      mChanged <- runDB $ getBookmark bidChanged
      liftIO $ (mSame >>= bookmarkArchiveHref) `shouldBe` Just "https://archive.org/same"
      liftIO $ (mChanged >>= bookmarkArchiveHref) `shouldBe` Nothing

    it "replaces existing tags on update rather than merging" $ do
      (uid, bid, bm') <- runDB $ do
        uid <- createTestUser "testuser"
        bid <- createBm uid "https://a.com" Nothing
        insert_ $ BookmarkTag uid "oldtag" bid 1
        bm' <- mkNewBm uid "https://a.com"
        return (uid, bid, bm')
      _ <- runDB $ upsertBookmarks uid [Just bid] [bm'] [["newtag"]]
      tags <- runDB $ getBookmarkTags bid
      liftIO $ tags `shouldBe` ["newtag"]

    it "returns Failed \"not found\" for an explicit id that does not exist" $ do
      (uid, bm') <- runDB $ do
        uid <- createTestUser "testuser"
        bm' <- mkNewBm uid "https://a.com"
        return (uid, bm')
      let bogusBid = toSqlKey 999999
      results <- runDB $ upsertBookmarks uid [Just bogusBid] [bm'] [[]]
      liftIO $ results `shouldBe` [Failed ReasonNotFound]

    it "returns Failed \"unauthorized\" for an explicit id owned by another user" $ do
      (uid1, bid2, bm') <- runDB $ do
        uid1 <- createTestUser "testuser"
        uid2 <- createTestUser "otheruser"
        bid2 <- createBm uid2 "https://other.com" Nothing
        bm' <- mkNewBm uid1 "https://a.com"
        return (uid1, bid2, bm')
      results <- runDB $ upsertBookmarks uid1 [Just bid2] [bm'] [[]]
      liftIO $ results `shouldBe` [Failed ReasonUnauthorized]

    it "returns Failed instead of crashing when an explicit id's new href collides with another existing bookmark" $ do
      (uid, bidTarget, bidOther, bm') <- runDB $ do
        uid <- createTestUser "testuser"
        bidTarget <- createBm uid "https://target.com" Nothing
        bidOther <- createBm uid "https://other.com" Nothing
        bm' <- mkNewBm uid "https://other.com"
        return (uid, bidTarget, bidOther, bm')
      results <- runDB $ upsertBookmarks uid [Just bidTarget] [bm'] [[]]
      liftIO $ results `shouldBe` [Failed ReasonHrefUsedByOther]
      mTarget <- runDB $ getBookmark bidTarget
      mOther <- runDB $ getBookmark bidOther
      liftIO $ fmap bookmarkHref mTarget `shouldBe` Just "https://target.com"
      liftIO $ fmap bookmarkHref mOther `shouldBe` Just "https://other.com"

    it "fails only the item whose userId does not match, leaving the rest of the batch unaffected" $ do
      (uid1, bm1, bm2) <- runDB $ do
        uid1 <- createTestUser "testuser"
        uid2 <- createTestUser "otheruser"
        bm1 <- mkNewBm uid1 "https://a.com"
        bm2 <- mkNewBm uid2 "https://b.com"
        return (uid1, bm1, bm2)
      results <- runDB $ upsertBookmarks uid1 [Nothing, Nothing] [bm1, bm2] [[], []]
      case results of
        [Created _, Failed ReasonUnauthorized] -> pure ()
        other -> liftIO $ expectationFailure ("unexpected results: " <> show other)

    it "collapses duplicate urls among new inserts into one row: first is Created, rest are Updated" $ do
      (uid, bm1, bm2, bm3) <- runDB $ do
        uid <- createTestUser "testuser"
        bm1 <- mkNewBm uid "https://dup.com"
        bm2 <- (\b -> b {bookmarkDescription = "second"}) <$> mkNewBm uid "https://dup.com"
        bm3 <- (\b -> b {bookmarkDescription = "third"}) <$> mkNewBm uid "https://dup.com"
        return (uid, bm1, bm2, bm3)
      results <-
        runDB $
          upsertBookmarks uid [Nothing, Nothing, Nothing] [bm1, bm2, bm3] [["a"], ["b"], ["c"]]
      case results of
        [Created bid1, Updated bid2, Updated bid3] -> do
          liftIO $ [bid2, bid3] `shouldBe` [bid1, bid1]
          mBm <- runDB $ getBookmark bid1
          liftIO $ fmap bookmarkDescription mBm `shouldBe` Just "third"
          tags <- runDB $ getBookmarkTags bid1
          liftIO $ tags `shouldBe` ["c"]
        other -> liftIO $ expectationFailure ("unexpected results: " <> show other)

    it "duplicate urls targeting an existing bookmark are all reported as updates" $ do
      (uid, bid, bm1, bm2) <- runDB $ do
        uid <- createTestUser "testuser"
        bid <- createBm uid "https://existing.com" Nothing
        bm1 <- mkNewBm uid "https://existing.com"
        bm2 <- (\b -> b {bookmarkDescription = "second"}) <$> mkNewBm uid "https://existing.com"
        return (uid, bid, bm1, bm2)
      results <- runDB $ upsertBookmarks uid [Nothing, Nothing] [bm1, bm2] [["a"], ["b"]]
      liftIO $ results `shouldBe` [Updated bid, Updated bid]
      mBm <- runDB $ getBookmark bid
      liftIO $ fmap bookmarkDescription mBm `shouldBe` Just "second"
      tags <- runDB $ getBookmarkTags bid
      liftIO $ tags `shouldBe` ["b"]

    it "processes a mixed batch of inserts, updates, and failures preserving input order" $ do
      (uid, bidUpdate, bmInsert, bmUpdate, bmBogus) <- runDB $ do
        uid <- createTestUser "testuser"
        bidUpdate <- createBm uid "https://existing.com" Nothing
        bmInsert <- mkNewBm uid "https://new.com"
        bmUpdate <- mkNewBm uid "https://existing.com"
        bmBogus <- mkNewBm uid "https://bogus.com"
        return (uid, bidUpdate, bmInsert, bmUpdate, bmBogus)
      let bogusBid = toSqlKey 999999
      results <-
        runDB $
          upsertBookmarks
            uid
            [Nothing, Just bidUpdate, Just bogusBid]
            [bmInsert, bmUpdate, bmBogus]
            [["a"], ["b"], ["c"]]
      case results of
        [Created _, Updated updatedBid, Failed ReasonNotFound] ->
          liftIO $ updatedBid `shouldBe` bidUpdate
        other -> liftIO $ expectationFailure ("unexpected results: " <> show other)
