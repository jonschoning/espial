{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.BookmarksSortSpec (spec) where

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

createBmAt :: Key User -> Text -> Text -> UTCTime -> DB (Key Bookmark)
createBmAt uid href title t = do
  slug <- liftIO mkBmSlug
  insert $ Bookmark uid slug href title "" t False False False Nothing

tagBm :: Key User -> Key Bookmark -> Text -> Int -> DB ()
tagBm uid bid tag seq' = insert_ $ BookmarkTag uid tag bid seq'

queryOrder :: Key User -> Bool -> BookmarkSort -> DB [Key Bookmark]
queryOrder uid isowner bsort = do
  (_, rows, _, _) <- bookmarksTagsQuery uid isowner SharedAll FilterAll [] Nothing (mkBookmarkPaging bsort Nothing 1) 100
  return $ map (entityKey . fst) rows

queryPageFlags :: Key User -> BookmarkSort -> Limit -> Page -> DB (Bool, Bool)
queryPageFlags uid bsort lim pg = do
  (_, _, hasEarlier, hasLater) <- bookmarksTagsQuery uid True SharedAll FilterAll [] Nothing (mkBookmarkPaging bsort Nothing pg) lim
  return (hasEarlier, hasLater)

spec :: Spec
spec = withApp $ do
  describe "bookmarksTagsQuery sort" $ do
    -- ─── BookmarkSortTime ────────────────────────────────────────────────

    describe "BookmarkSortTime" $ do
      it "SortAsc orders oldest first" $ do
        (uid, bOld, bNew) <- runDB $ do
          uid <- createTestUser
          bNew <- createBmAt uid "https://new.com" "new" (addUTCTime 100 t0)
          bOld <- createBmAt uid "https://old.com" "old" t0
          return (uid, bOld, bNew)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortTime SortAsc)
        liftIO $ bids `shouldBe` [bOld, bNew]

      it "SortDesc orders newest first" $ do
        (uid, bOld, bNew) <- runDB $ do
          uid <- createTestUser
          bNew <- createBmAt uid "https://new.com" "new" (addUTCTime 100 t0)
          bOld <- createBmAt uid "https://old.com" "old" t0
          return (uid, bOld, bNew)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortTime SortDesc)
        liftIO $ bids `shouldBe` [bNew, bOld]

    -- ─── BookmarkSortTitle ───────────────────────────────────────────────

    describe "BookmarkSortTitle" $ do
      it "SortAsc orders case-insensitively" $ do
        -- byte/ASCII ordering would put "Zebra" ('Z'=90) before "apple"
        -- ('a'=97); a correct case-insensitive sort must not do that
        (uid, bApple, bZebra) <- runDB $ do
          uid <- createTestUser
          bZebra <- createBmAt uid "https://z.com" "Zebra" t0
          bApple <- createBmAt uid "https://a.com" "apple" t0
          return (uid, bApple, bZebra)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortTitle SortAsc)
        liftIO $ bids `shouldBe` [bApple, bZebra]

      it "SortDesc reverses the order" $ do
        (uid, bApple, bZebra) <- runDB $ do
          uid <- createTestUser
          bZebra <- createBmAt uid "https://z.com" "Zebra" t0
          bApple <- createBmAt uid "https://a.com" "apple" t0
          return (uid, bApple, bZebra)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortTitle SortDesc)
        liftIO $ bids `shouldBe` [bZebra, bApple]

    -- ─── BookmarkSortNumTags ─────────────────────────────────────────────

    describe "BookmarkSortNumTags" $ do
      it "SortAsc orders fewest tags first, including untagged bookmarks" $ do
        (uid, bNone, bOne, bTwo) <- runDB $ do
          uid <- createTestUser
          bTwo <- createBmAt uid "https://two.com" "two" t0
          bNone <- createBmAt uid "https://none.com" "none" t0
          bOne <- createBmAt uid "https://one.com" "one" t0
          tagBm uid bTwo "a" 1
          tagBm uid bTwo "b" 2
          tagBm uid bOne "a" 1
          return (uid, bNone, bOne, bTwo)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortNumTags SortAsc)
        liftIO $ bids `shouldBe` [bNone, bOne, bTwo]

      it "SortDesc orders most tags first" $ do
        (uid, bNone, bOne, bTwo) <- runDB $ do
          uid <- createTestUser
          bTwo <- createBmAt uid "https://two.com" "two" t0
          bNone <- createBmAt uid "https://none.com" "none" t0
          bOne <- createBmAt uid "https://one.com" "one" t0
          tagBm uid bTwo "a" 1
          tagBm uid bTwo "b" 2
          tagBm uid bOne "a" 1
          return (uid, bNone, bOne, bTwo)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortNumTags SortDesc)
        liftIO $ bids `shouldBe` [bTwo, bOne, bNone]

      it "excludes dot-prefixed tags from the count for non-owners" $ do
        -- bFew: 2 public tags. bMany: 1 public + 2 dot-prefixed (private) tags.
        -- owner sees bMany (3) > bFew (2); a non-owner, with private tags
        -- stripped, must see bMany (1) < bFew (2) -- the order should flip.
        (uid, bFew, bMany) <- runDB $ do
          uid <- createTestUser
          bFew <- createBmAt uid "https://few.com" "few" t0
          bMany <- createBmAt uid "https://many.com" "many" t0
          tagBm uid bFew "public1" 1
          tagBm uid bFew "public2" 2
          tagBm uid bMany "public1" 1
          tagBm uid bMany ".secret1" 2
          tagBm uid bMany ".secret2" 3
          return (uid, bFew, bMany)
        ownerOrder <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortNumTags SortAsc)
        liftIO $ ownerOrder `shouldBe` [bFew, bMany]
        nonOwnerOrder <- runDB $ queryOrder uid False (BookmarkSort BookmarkSortNumTags SortAsc)
        liftIO $ nonOwnerOrder `shouldBe` [bMany, bFew]

    -- ─── BookmarkSortUrl ─────────────────────────────────────────────────

    describe "BookmarkSortUrl" $ do
      it "SortAsc orders by href with the scheme ignored" $ do
        -- byte order by full href would put "ftp://" before "http://"
        -- before "https://"; sorting on the post-scheme remainder must not
        (uid, bAlpha, bBravo, bCharlie) <- runDB $ do
          uid <- createTestUser
          bCharlie <- createBmAt uid "https://charlie.com/p" "c" t0
          bAlpha <- createBmAt uid "ftp://alpha.com/p" "a" t0
          bBravo <- createBmAt uid "http://bravo.com/p" "b" t0
          return (uid, bAlpha, bBravo, bCharlie)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortUrl SortAsc)
        liftIO $ bids `shouldBe` [bAlpha, bBravo, bCharlie]

      it "SortDesc reverses the order" $ do
        (uid, bAlpha, bBravo, bCharlie) <- runDB $ do
          uid <- createTestUser
          bCharlie <- createBmAt uid "https://charlie.com/p" "c" t0
          bAlpha <- createBmAt uid "ftp://alpha.com/p" "a" t0
          bBravo <- createBmAt uid "http://bravo.com/p" "b" t0
          return (uid, bAlpha, bBravo, bCharlie)
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortUrl SortDesc)
        liftIO $ bids `shouldBe` [bCharlie, bBravo, bAlpha]

      it "sorts a schemeless href by its full value" $ do
        (uid, bNoScheme, bWithScheme) <- runDB $ do
          uid <- createTestUser
          bWithScheme <- createBmAt uid "https://zzz.com" "z" t0
          bNoScheme <- createBmAt uid "aaa-no-scheme" "n" t0
          return (uid, bNoScheme, bWithScheme)
        -- "aaa-no-scheme" sorts before the scheme-stripped "zzz.com"
        bids <- runDB $ queryOrder uid True (BookmarkSort BookmarkSortUrl SortAsc)
        liftIO $ bids `shouldBe` [bNoScheme, bWithScheme]

    -- ─── cursor paging is only defined for time sorts ─────────────────────

    describe "before/after cursor paging" $ do
      it "has no filtering effect when sorting by a non-default field" $ do
        (uid, bOld, bNew) <- runDB $ do
          uid <- createTestUser
          bOld <- createBmAt uid "https://old.com" "aaa" t0
          bNew <- createBmAt uid "https://new.com" "bbb" (addUTCTime 100 t0)
          return (uid, bOld, bNew)
        -- a "before t0" cursor would normally exclude bOld under time sort;
        -- under title sort mkBookmarkPaging must discard it entirely
        (_, rows, _, _) <-
          runDB
            $ bookmarksTagsQuery
              uid
              True
              SharedAll
              FilterAll
              []
              Nothing
              (mkBookmarkPaging (BookmarkSort BookmarkSortTitle SortAsc) (Just (PagingCursorBefore (BookmarkCursor t0 Nothing))) 1)
              100
        let bids = map (entityKey . fst) rows
        liftIO $ bids `shouldBe` [bOld, bNew]

      it "does not reverse results when an after-cursor is given with a non-default sort" $ do
        -- an "after t0" cursor would normally exclude bOld and, on the
        -- PagingCursorAfter path, trigger a reverse() to restore display
        -- order; under title sort neither must happen
        (uid, bOld, bNew) <- runDB $ do
          uid <- createTestUser
          bOld <- createBmAt uid "https://old.com" "aaa" t0
          bNew <- createBmAt uid "https://new.com" "bbb" (addUTCTime 100 t0)
          return (uid, bOld, bNew)
        (_, rows, _, _) <-
          runDB
            $ bookmarksTagsQuery
              uid
              True
              SharedAll
              FilterAll
              []
              Nothing
              (mkBookmarkPaging (BookmarkSort BookmarkSortTitle SortAsc) (Just (PagingCursorAfter (BookmarkCursor t0 Nothing))) 1)
              100
        let bids = map (entityKey . fst) rows
        liftIO $ bids `shouldBe` [bOld, bNew]

    -- ─── hasEarlier/hasLater fallback (page/offset) paging ─────────────────

    describe "hasEarlier/hasLater for non-default sorts (page/offset fallback)" $ do
      it "first page with more rows beyond the limit: hasEarlier=False, hasLater=True" $ do
        uid <- runDB $ do
          uid <- createTestUser
          mapM_
            (\n -> void $ createBmAt uid ("https://" <> tshow (n :: Int) <> ".com") (tshow n) t0)
            [1 .. 3]
          return uid
        (hasEarlier, hasLater) <- runDB $ queryPageFlags uid (BookmarkSort BookmarkSortTitle SortAsc) 2 1
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` True

      it "last page: hasEarlier=True, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          mapM_
            (\n -> void $ createBmAt uid ("https://" <> tshow (n :: Int) <> ".com") (tshow n) t0)
            [1 .. 3]
          return uid
        (hasEarlier, hasLater) <- runDB $ queryPageFlags uid (BookmarkSort BookmarkSortTitle SortAsc) 2 2
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` False

      it "single page, all rows fit: hasEarlier=False, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          _ <- createBmAt uid "https://a.com" "a" t0
          return uid
        (hasEarlier, hasLater) <- runDB $ queryPageFlags uid (BookmarkSort BookmarkSortTitle SortAsc) 100 1
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` False
