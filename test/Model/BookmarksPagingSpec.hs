{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.BookmarksPagingSpec (spec) where

import Data.Time (addUTCTime)
import Model.Custom (hashPasswordBCryptWithPolicy, mkBmSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User "testuser" pwHash Nothing False False True False False True Nothing

createBmAt :: Key User -> Text -> UTCTime -> DB (Key Bookmark)
createBmAt uid href t = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href "desc" "" t False False False Nothing

queryWithCursor :: Key User -> Maybe BookmarkPagingCursorTime -> DB [Key Bookmark]
queryWithCursor uid mcursor = do
  (_, rows, _, _) <- bookmarksTagsQuery uid True SharedAll FilterAll [] Nothing mcursor 100 1
  return $ map (entityKey . fst) rows

queryFlags :: Key User -> Maybe BookmarkPagingCursorTime -> Limit -> DB (Bool, Bool)
queryFlags uid mcursor lim = do
  (_, _, hasEarlier, hasLater) <- bookmarksTagsQuery uid True SharedAll FilterAll [] Nothing mcursor lim 1
  return (hasEarlier, hasLater)

spec :: Spec
spec = withApp $ do
  describe "bookmarksTagsQuery paging cursor" $ do

    -- ─── PagingCursorBefore (lt) ───────────────────────────────────────────

    describe "PagingCursorBefore (lt boundary)" $ do
      it "excludes a bookmark at exactly the cursor time" $ do
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" t0
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore t0))
        liftIO $ bids `shouldNotContain` [bid]

      it "includes a bookmark strictly before the cursor time" $ do
        let tBefore = t0
            tCursor = addUTCTime 1 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBefore
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore tCursor))
        liftIO $ bids `shouldContain` [bid]

      it "excludes a bookmark strictly after the cursor time" $ do
        let tAfter = addUTCTime 1 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tAfter
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore t0))
        liftIO $ bids `shouldNotContain` [bid]

    -- ─── PagingCursorAfter (gt) ────────────────────────────────────────────

    describe "PagingCursorAfter (gt boundary)" $ do
      it "excludes a bookmark at exactly the cursor time" $ do
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" t0
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter t0))
        liftIO $ bids `shouldNotContain` [bid]

      it "includes a bookmark strictly after the cursor time" $ do
        let tAfter = addUTCTime 1 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tAfter
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter t0))
        liftIO $ bids `shouldContain` [bid]

      it "excludes a bookmark strictly before the cursor time" $ do
        let tCursor = addUTCTime 10 t0
            tBefore = addUTCTime 9 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBefore
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter tCursor))
        liftIO $ bids `shouldNotContain` [bid]

    -- ─── Fractional second cursor vs whole-second bookmark ─────────────────

    describe "fractional-second cursor vs whole-second bookmark" $ do
      it "PagingCursorBefore: includes bookmark at whole second below a fractional cursor" $ do
        -- cursor at 00:00:01.5, bookmark at 00:00:01 → bookmark is before cursor
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1.5
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore tCursor))
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorBefore: excludes bookmark at whole second above a fractional cursor" $ do
        -- cursor at 00:00:00.5, bookmark at 00:00:01 → bookmark is after cursor
        let tCursor = UTCTime (fromGregorian 2024 1 1) 0.5
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore tCursor))
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorAfter: includes bookmark at whole second above a fractional cursor" $ do
        -- cursor at 00:00:00.5, bookmark at 00:00:01 → bookmark is after cursor
        let tCursor = UTCTime (fromGregorian 2024 1 1) 0.5
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter tCursor))
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorAfter: excludes bookmark at whole second below a fractional cursor" $ do
        -- cursor at 00:00:01.5, bookmark at 00:00:01 → bookmark is before cursor
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1.5
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter tCursor))
        liftIO $ bids `shouldNotContain` [bid]

    -- ─── Whole-second cursor vs fractional-second bookmark ─────────────────

    describe "whole-second cursor vs fractional-second bookmark" $ do
      it "PagingCursorBefore: excludes fractional-second bookmark at the cursor time" $ do
        -- cursor at 00:00:01, bookmark at 00:00:01.0 (same instant, fractional form)
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore tCursor))
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorBefore: includes bookmark at fractional second just before a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:00.999 → bookmark is just before
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 0.999
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore tCursor))
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorBefore: excludes bookmark at fractional second just after a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:01.001 → bookmark is just after
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 1.001
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore tCursor))
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorAfter: excludes fractional-second bookmark at the cursor time" $ do
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter tCursor))
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorAfter: includes bookmark at fractional second just after a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:01.001 → bookmark is just after
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 1.001
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter tCursor))
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorAfter: excludes bookmark at fractional second just before a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:00.999 → bookmark is just before
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 0.999
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter tCursor))
        liftIO $ bids `shouldNotContain` [bid]

    -- ─── hasEarlier / hasLater navigation flags ────────────────────────────

    describe "hasEarlier and hasLater navigation flags" $ do
      it "no cursor, all rows fit: hasEarlier=False, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://a.com" t0
          return uid
        (hasEarlier, hasLater) <- runDB $ queryFlags uid Nothing 100
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` False

      it "no cursor, rows exceed limit: hasEarlier=True, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          mapM_
            (\n -> void $ createBmAt uid ("https://" <> tshow n <> ".com") (addUTCTime (fromIntegral (n :: Int)) t0))
            [1 .. 3]
          return uid
        (hasEarlier, hasLater) <- runDB $ queryFlags uid Nothing 2
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` False

      it "PagingCursorBefore with results, all fit: hasLater=True, hasEarlier=False" $ do
        -- tBefore is at t0, cursor is at t0+10; bookmark is before cursor
        uid <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://before.com" t0
          return uid
        let tCursor = addUTCTime 10 t0
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (Just (PagingCursorBefore tCursor)) 100
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` True

      it "PagingCursorBefore with rows exceeding limit: hasEarlier=True, hasLater=True" $ do
        uid <- runDB $ do
          uid <- createTestUser
          mapM_
            (\n -> void $ createBmAt uid ("https://" <> tshow n <> ".com") (addUTCTime (fromIntegral (n :: Int)) t0))
            [1 .. 3]
          return uid
        -- cursor is past all 3 bookmarks; fetch only 2 of the 3
        let tCursor = addUTCTime 10 t0
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (Just (PagingCursorBefore tCursor)) 2
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` True

      it "PagingCursorAfter with results, all fit: hasEarlier=True, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://after.com" (addUTCTime 10 t0)
          return uid
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (Just (PagingCursorAfter t0)) 100
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` False

      it "PagingCursorAfter with rows exceeding limit: hasEarlier=True, hasLater=True" $ do
        uid <- runDB $ do
          uid <- createTestUser
          mapM_
            (\n -> void $ createBmAt uid ("https://" <> tshow n <> ".com") (addUTCTime (fromIntegral (n :: Int)) t0))
            [1 .. 3]
          return uid
        -- cursor is before all 3 bookmarks; fetch only 2 of the 3
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (Just (PagingCursorAfter (addUTCTime (-1) t0))) 2
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` True
