{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.NotesSortSpec (spec) where

import Data.Time (addUTCTime)
import Model.Custom (hashPasswordBCryptWithPolicy, mkNtSlug)
import TestImport
import Types (DB)

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2024 1 1) 0

createTestUser :: DB (Key User)
createTestUser = do
  pwHash <- liftIO $ hashPasswordBCryptWithPolicy bcryptTestPolicy "pass"
  insert $ User "testuser" pwHash Nothing False False True True False False True Nothing

createNoteAt :: Key User -> Text -> UTCTime -> DB (Key Note)
createNoteAt uid title t = do
  slug <- liftIO mkNtSlug
  insert $ Note uid slug (length title) title "text" False False t t

queryOrder :: Key User -> NoteSort -> DB [Key Note]
queryOrder uid nsort = do
  (_, rows, _, _) <- getNoteList uid Nothing SharedAll (mkNotePaging nsort Nothing 1) 100
  return $ map entityKey rows

queryPageFlags :: Key User -> NoteSort -> Limit -> Page -> DB (Bool, Bool)
queryPageFlags uid nsort lim pg = do
  (_, _, hasEarlier, hasLater) <- getNoteList uid Nothing SharedAll (mkNotePaging nsort Nothing pg) lim
  return (hasEarlier, hasLater)

spec :: Spec
spec = withApp $ do
  describe "getNoteList sort" $ do
    -- ─── NoteSortCreated ─────────────────────────────────────────────────

    describe "NoteSortCreated" $ do
      it "SortAsc orders oldest first" $ do
        (uid, nOld, nNew) <- runDB $ do
          uid <- createTestUser
          nNew <- createNoteAt uid "new" (addUTCTime 100 t0)
          nOld <- createNoteAt uid "old" t0
          return (uid, nOld, nNew)
        nids <- runDB $ queryOrder uid (NoteSort NoteSortCreated SortAsc)
        liftIO $ nids `shouldBe` [nOld, nNew]

      it "SortDesc orders newest first" $ do
        (uid, nOld, nNew) <- runDB $ do
          uid <- createTestUser
          nNew <- createNoteAt uid "new" (addUTCTime 100 t0)
          nOld <- createNoteAt uid "old" t0
          return (uid, nOld, nNew)
        nids <- runDB $ queryOrder uid (NoteSort NoteSortCreated SortDesc)
        liftIO $ nids `shouldBe` [nNew, nOld]

    -- ─── NoteSortTitle ───────────────────────────────────────────────────

    describe "NoteSortTitle" $ do
      it "SortAsc orders case-insensitively" $ do
        -- byte/ASCII ordering would put "Zebra" ('Z'=90) before "apple"
        -- ('a'=97); a correct case-insensitive sort must not do that
        (uid, nApple, nZebra) <- runDB $ do
          uid <- createTestUser
          nZebra <- createNoteAt uid "Zebra" t0
          nApple <- createNoteAt uid "apple" t0
          return (uid, nApple, nZebra)
        nids <- runDB $ queryOrder uid (NoteSort NoteSortTitle SortAsc)
        liftIO $ nids `shouldBe` [nApple, nZebra]

      it "SortDesc reverses the order" $ do
        (uid, nApple, nZebra) <- runDB $ do
          uid <- createTestUser
          nZebra <- createNoteAt uid "Zebra" t0
          nApple <- createNoteAt uid "apple" t0
          return (uid, nApple, nZebra)
        nids <- runDB $ queryOrder uid (NoteSort NoteSortTitle SortDesc)
        liftIO $ nids `shouldBe` [nZebra, nApple]

    -- ─── cursor paging is only defined for created sorts ───────────────────

    describe "before/after cursor paging" $ do
      it "has no filtering effect when sorting by a non-default field" $ do
        (uid, nOld, nNew) <- runDB $ do
          uid <- createTestUser
          nOld <- createNoteAt uid "aaa" t0
          nNew <- createNoteAt uid "bbb" (addUTCTime 100 t0)
          return (uid, nOld, nNew)
        -- a "before t0" cursor would normally exclude nOld under created
        -- sort; under title sort mkNotePaging must discard it entirely
        (_, rows, _, _) <-
          runDB
            $ getNoteList
              uid
              Nothing
              SharedAll
              (mkNotePaging (NoteSort NoteSortTitle SortAsc) (Just (PagingCursorBefore t0)) 1)
              100
        let nids = map entityKey rows
        liftIO $ nids `shouldBe` [nOld, nNew]

      it "does not reverse results when an after-cursor is given with a non-default sort" $ do
        -- an "after t0" cursor would normally exclude nOld and, on the
        -- PagingCursorAfter path, trigger a reverse() to restore display
        -- order; under title sort neither must happen
        (uid, nOld, nNew) <- runDB $ do
          uid <- createTestUser
          nOld <- createNoteAt uid "aaa" t0
          nNew <- createNoteAt uid "bbb" (addUTCTime 100 t0)
          return (uid, nOld, nNew)
        (_, rows, _, _) <-
          runDB
            $ getNoteList
              uid
              Nothing
              SharedAll
              (mkNotePaging (NoteSort NoteSortTitle SortAsc) (Just (PagingCursorAfter t0)) 1)
              100
        let nids = map entityKey rows
        liftIO $ nids `shouldBe` [nOld, nNew]

    -- ─── hasEarlier/hasLater fallback (page/offset) paging ─────────────────

    describe "hasEarlier/hasLater for non-default sorts (page/offset fallback)" $ do
      it "first page with more rows beyond the limit: hasEarlier=False, hasLater=True" $ do
        uid <- runDB $ do
          uid <- createTestUser
          mapM_ (\n -> void $ createNoteAt uid (tshow (n :: Int)) t0) [1 .. 3]
          return uid
        (hasEarlier, hasLater) <- runDB $ queryPageFlags uid (NoteSort NoteSortTitle SortAsc) 2 1
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` True

      it "last page: hasEarlier=True, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          mapM_ (\n -> void $ createNoteAt uid (tshow (n :: Int)) t0) [1 .. 3]
          return uid
        (hasEarlier, hasLater) <- runDB $ queryPageFlags uid (NoteSort NoteSortTitle SortAsc) 2 2
        liftIO $ hasEarlier `shouldBe` True
        liftIO $ hasLater `shouldBe` False

      it "single page, all rows fit: hasEarlier=False, hasLater=False" $ do
        uid <- runDB $ do
          uid <- createTestUser
          _ <- createNoteAt uid "a" t0
          return uid
        (hasEarlier, hasLater) <- runDB $ queryPageFlags uid (NoteSort NoteSortTitle SortAsc) 100 1
        liftIO $ hasEarlier `shouldBe` False
        liftIO $ hasLater `shouldBe` False
