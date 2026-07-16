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
  insert $ User "testuser" pwHash Nothing False False True True False False True Nothing

createBmAt :: Key User -> Text -> UTCTime -> DB (Key Bookmark)
createBmAt uid href t = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href "desc" "" t False False False Nothing

queryDir :: SortDirection -> Key User -> Maybe BookmarkPagingCursor -> Limit -> DB ([Key Bookmark], Bool, Bool)
queryDir dir uid mcursor lim = do
  (_, rows, hasEarlier, hasLater) <- bookmarksTagsQuery uid True SharedAll FilterAll [] Nothing (PageByCursor dir mcursor) lim
  return (map (entityKey . fst) rows, hasEarlier, hasLater)

queryWithCursor :: Key User -> Maybe BookmarkPagingCursor -> DB [Key Bookmark]
queryWithCursor uid mcursor = do
  (bids, _, _) <- queryDir SortDesc uid mcursor 100
  return bids

queryFlags :: Key User -> Maybe BookmarkPagingCursor -> Limit -> DB (Bool, Bool)
queryFlags uid mcursor lim = do
  (_, hasEarlier, hasLater) <- queryDir SortDesc uid mcursor lim
  return (hasEarlier, hasLater)

beforeT :: UTCTime -> Maybe BookmarkPagingCursor
beforeT t = Just (PagingCursorBefore (BookmarkCursor t Nothing))

afterT :: UTCTime -> Maybe BookmarkPagingCursor
afterT t = Just (PagingCursorAfter (BookmarkCursor t Nothing))

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
        bids <- runDB $ queryWithCursor uid (beforeT t0)
        liftIO $ bids `shouldNotContain` [bid]

      it "includes a bookmark strictly before the cursor time" $ do
        let tBefore = t0
            tCursor = addUTCTime 1 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBefore
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (beforeT tCursor)
        liftIO $ bids `shouldContain` [bid]

      it "excludes a bookmark strictly after the cursor time" $ do
        let tAfter = addUTCTime 1 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tAfter
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (beforeT t0)
        liftIO $ bids `shouldNotContain` [bid]

    -- ─── PagingCursorAfter (gt) ────────────────────────────────────────────

    describe "PagingCursorAfter (gt boundary)" $ do
      it "excludes a bookmark at exactly the cursor time" $ do
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" t0
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT t0)
        liftIO $ bids `shouldNotContain` [bid]

      it "includes a bookmark strictly after the cursor time" $ do
        let tAfter = addUTCTime 1 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tAfter
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT t0)
        liftIO $ bids `shouldContain` [bid]

      it "excludes a bookmark strictly before the cursor time" $ do
        let tCursor = addUTCTime 10 t0
            tBefore = addUTCTime 9 t0
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBefore
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT tCursor)
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
        bids <- runDB $ queryWithCursor uid (beforeT tCursor)
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorBefore: excludes bookmark at whole second above a fractional cursor" $ do
        -- cursor at 00:00:00.5, bookmark at 00:00:01 → bookmark is after cursor
        let tCursor = UTCTime (fromGregorian 2024 1 1) 0.5
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (beforeT tCursor)
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorAfter: includes bookmark at whole second above a fractional cursor" $ do
        -- cursor at 00:00:00.5, bookmark at 00:00:01 → bookmark is after cursor
        let tCursor = UTCTime (fromGregorian 2024 1 1) 0.5
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT tCursor)
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorAfter: excludes bookmark at whole second below a fractional cursor" $ do
        -- cursor at 00:00:01.5, bookmark at 00:00:01 → bookmark is before cursor
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1.5
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT tCursor)
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
        bids <- runDB $ queryWithCursor uid (beforeT tCursor)
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorBefore: includes bookmark at fractional second just before a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:00.999 → bookmark is just before
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 0.999
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (beforeT tCursor)
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorBefore: excludes bookmark at fractional second just after a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:01.001 → bookmark is just after
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 1.001
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (beforeT tCursor)
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorAfter: excludes fractional-second bookmark at the cursor time" $ do
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 1
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT tCursor)
        liftIO $ bids `shouldNotContain` [bid]

      it "PagingCursorAfter: includes bookmark at fractional second just after a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:01.001 → bookmark is just after
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 1.001
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT tCursor)
        liftIO $ bids `shouldContain` [bid]

      it "PagingCursorAfter: excludes bookmark at fractional second just before a whole-second cursor" $ do
        -- cursor at 00:00:01, bookmark at 00:00:00.999 → bookmark is just before
        let tCursor = UTCTime (fromGregorian 2024 1 1) 1
            tBm = UTCTime (fromGregorian 2024 1 1) 0.999
        (uid, bid) <- runDB $ do
          uid <- createTestUser
          bid <- createBmAt uid "https://a.com" tBm
          return (uid, bid)
        bids <- runDB $ queryWithCursor uid (afterT tCursor)
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
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (beforeT tCursor) 100
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
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (beforeT tCursor) 2
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` True

      it "PagingCursorAfter with results, all fit: hasEarlier=True, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://after.com" (addUTCTime 10 t0)
          return uid
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (afterT t0) 100
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
        (hasEarlier, hasLater) <- runDB $ queryFlags uid (afterT (addUTCTime (-1) t0)) 2
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` True

    -- ─── Composite (time, id) keyset tie-breaking ──────────────────────────

    describe "composite (time, id) cursor tie-breaking" $ do
      it "PagingCursorBefore keeps only same-time rows with a smaller id" $ do
        (uid, b1, b2, _) <- runDB $ do
          uid <- createTestUser
          b1 <- createBmAt uid "https://1.com" t0
          b2 <- createBmAt uid "https://2.com" t0
          b3 <- createBmAt uid "https://3.com" t0
          return (uid, b1, b2, b3)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore (BookmarkCursor t0 (Just b2))))
        liftIO $ bids `shouldBe` [b1]

      it "PagingCursorAfter keeps only same-time rows with a larger id" $ do
        (uid, b2, b3) <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://1.com" t0
          b2 <- createBmAt uid "https://2.com" t0
          b3 <- createBmAt uid "https://3.com" t0
          return (uid, b2, b3)
        bids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter (BookmarkCursor t0 (Just b2))))
        liftIO $ bids `shouldBe` [b3]

      it "a time-only cursor still uses the pure time boundary" $ do
        (uid, bOld) <- runDB $ do
          uid <- createTestUser
          bOld <- createBmAt uid "https://old.com" (addUTCTime (-1) t0)
          _ <- createBmAt uid "https://tied.com" t0
          return (uid, bOld)
        bids <- runDB $ queryWithCursor uid (beforeT t0)
        liftIO $ bids `shouldBe` [bOld]

      it "pages through rows sharing one timestamp without skips or repeats" $ do
        (uid, allBids) <- runDB $ do
          uid <- createTestUser
          bids <- mapM (\n -> createBmAt uid ("https://" <> tshow (n :: Int) <> ".com") t0) [1 .. 5]
          return (uid, bids)
        -- newest-first display of same-time rows falls back to id desc
        let walk acc mcursor = do
              (bids, _, _) <- runDB $ queryDir SortDesc uid mcursor 2
              case lastMay bids of
                Nothing -> pure acc
                Just lastBid -> walk (acc <> bids) (Just (PagingCursorBefore (BookmarkCursor t0 (Just lastBid))))
        collected <- walk [] Nothing
        liftIO $ collected `shouldBe` reverse allBids

    -- ─── Asc display direction (PageByCursor SortAsc) ──────────────────────

    describe "asc display direction (PageByCursor SortAsc)" $ do
      it "no cursor returns oldest first with hasEarlier=False, hasLater=True when rows exceed the limit" $ do
        (uid, b1, b2, _) <- runDB $ do
          uid <- createTestUser
          b1 <- createBmAt uid "https://1.com" (addUTCTime 1 t0)
          b2 <- createBmAt uid "https://2.com" (addUTCTime 2 t0)
          b3 <- createBmAt uid "https://3.com" (addUTCTime 3 t0)
          return (uid, b1, b2, b3)
        (bids, hasEarlier, hasLater) <- runDB $ queryDir SortAsc uid Nothing 2
        liftIO $ bids `shouldBe` [b1, b2]
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` True

      it "PagingCursorAfter returns the adjacent newer rows in ascending order" $ do
        (uid, b2, b3, _) <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://1.com" (addUTCTime 1 t0)
          b2 <- createBmAt uid "https://2.com" (addUTCTime 2 t0)
          b3 <- createBmAt uid "https://3.com" (addUTCTime 3 t0)
          b4 <- createBmAt uid "https://4.com" (addUTCTime 4 t0)
          return (uid, b2, b3, b4)
        (bids, hasEarlier, hasLater) <- runDB $ queryDir SortAsc uid (afterT (addUTCTime 1 t0)) 2
        liftIO $ bids `shouldBe` [b2, b3]
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` True

      it "PagingCursorBefore returns the adjacent older rows in ascending order" $ do
        -- three rows below the cursor with limit 2: the scan must take the
        -- two adjacent to the cursor (b2, b3), displayed ascending
        (uid, b2, b3) <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://1.com" (addUTCTime 1 t0)
          b2 <- createBmAt uid "https://2.com" (addUTCTime 2 t0)
          b3 <- createBmAt uid "https://3.com" (addUTCTime 3 t0)
          _ <- createBmAt uid "https://4.com" (addUTCTime 4 t0)
          return (uid, b2, b3)
        (bids, hasEarlier, hasLater) <- runDB $ queryDir SortAsc uid (beforeT (addUTCTime 4 t0)) 2
        liftIO $ bids `shouldBe` [b2, b3]
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` True

      it "PagingCursorBefore with all older rows fitting: hasEarlier=False, hasLater=True" $ do
        (uid, b1) <- runDB $ do
          uid <- createTestUser
          b1 <- createBmAt uid "https://1.com" (addUTCTime 1 t0)
          _ <- createBmAt uid "https://2.com" (addUTCTime 2 t0)
          return (uid, b1)
        (bids, hasEarlier, hasLater) <- runDB $ queryDir SortAsc uid (beforeT (addUTCTime 2 t0)) 100
        liftIO $ bids `shouldBe` [b1]
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` True

      it "pages forward through rows sharing one timestamp without skips or repeats" $ do
        (uid, allBids) <- runDB $ do
          uid <- createTestUser
          bids <- mapM (\n -> createBmAt uid ("https://" <> tshow (n :: Int) <> ".com") t0) [1 .. 5]
          return (uid, bids)
        -- oldest-first display of same-time rows falls back to id asc
        let walk acc mcursor = do
              (bids, _, _) <- runDB $ queryDir SortAsc uid mcursor 2
              case lastMay bids of
                Nothing -> pure acc
                Just lastBid -> walk (acc <> bids) (Just (PagingCursorAfter (BookmarkCursor t0 (Just lastBid))))
        collected <- walk [] Nothing
        liftIO $ collected `shouldBe` allBids
