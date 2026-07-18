{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.NotesPagingSpec (spec) where

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

queryDir :: SortDirection -> Key User -> Maybe NotePagingCursor -> Limit -> DB ([Key Note], Bool, Bool)
queryDir dir uid mcursor lim = do
  (_, rows, hasEarlier, hasLater) <- getNoteList uid Nothing SharedAll (PageByCursor dir mcursor) lim
  return (map entityKey rows, hasEarlier, hasLater)

queryWithCursor :: Key User -> Maybe NotePagingCursor -> DB [Key Note]
queryWithCursor uid mcursor = do
  (nids, _, _) <- queryDir SortDesc uid mcursor 100
  return nids

beforeT :: UTCTime -> Maybe NotePagingCursor
beforeT t = Just (PagingCursorBefore (NoteCursor t Nothing))

afterT :: UTCTime -> Maybe NotePagingCursor
afterT t = Just (PagingCursorAfter (NoteCursor t Nothing))

spec :: Spec
spec = withApp $ do
  describe "getNoteList paging cursor" $ do

    -- ─── Cursor boundaries ─────────────────────────────────────────────────

    describe "cursor boundaries" $ do
      it "PagingCursorBefore excludes a note at exactly the cursor time" $ do
        (uid, nid) <- runDB $ do
          uid <- createTestUser
          nid <- createNoteAt uid "a" t0
          return (uid, nid)
        nids <- runDB $ queryWithCursor uid (beforeT t0)
        liftIO $ nids `shouldNotContain` [nid]

      it "PagingCursorBefore includes a note strictly before the cursor time" $ do
        (uid, nid) <- runDB $ do
          uid <- createTestUser
          nid <- createNoteAt uid "a" t0
          return (uid, nid)
        nids <- runDB $ queryWithCursor uid (beforeT (addUTCTime 1 t0))
        liftIO $ nids `shouldContain` [nid]

      it "PagingCursorAfter excludes a note at exactly the cursor time" $ do
        (uid, nid) <- runDB $ do
          uid <- createTestUser
          nid <- createNoteAt uid "a" t0
          return (uid, nid)
        nids <- runDB $ queryWithCursor uid (afterT t0)
        liftIO $ nids `shouldNotContain` [nid]

      it "PagingCursorAfter includes a note strictly after the cursor time" $ do
        (uid, nid) <- runDB $ do
          uid <- createTestUser
          nid <- createNoteAt uid "a" (addUTCTime 1 t0)
          return (uid, nid)
        nids <- runDB $ queryWithCursor uid (afterT t0)
        liftIO $ nids `shouldContain` [nid]

    -- ─── Composite (time, id) keyset tie-breaking ──────────────────────────

    describe "composite (time, id) cursor tie-breaking" $ do
      it "PagingCursorBefore keeps only same-time rows with a smaller id" $ do
        (uid, n1, n2, _) <- runDB $ do
          uid <- createTestUser
          n1 <- createNoteAt uid "1" t0
          n2 <- createNoteAt uid "2" t0
          n3 <- createNoteAt uid "3" t0
          return (uid, n1, n2, n3)
        nids <- runDB $ queryWithCursor uid (Just (PagingCursorBefore (NoteCursor t0 (Just n2))))
        liftIO $ nids `shouldBe` [n1]

      it "PagingCursorAfter keeps only same-time rows with a larger id" $ do
        (uid, n2, n3) <- runDB $ do
          uid <- createTestUser
          _ <- createNoteAt uid "1" t0
          n2 <- createNoteAt uid "2" t0
          n3 <- createNoteAt uid "3" t0
          return (uid, n2, n3)
        nids <- runDB $ queryWithCursor uid (Just (PagingCursorAfter (NoteCursor t0 (Just n2))))
        liftIO $ nids `shouldBe` [n3]

      it "a time-only cursor still uses the pure time boundary" $ do
        (uid, nOld) <- runDB $ do
          uid <- createTestUser
          nOld <- createNoteAt uid "old" (addUTCTime (-1) t0)
          _ <- createNoteAt uid "tied" t0
          return (uid, nOld)
        nids <- runDB $ queryWithCursor uid (beforeT t0)
        liftIO $ nids `shouldBe` [nOld]

      it "pages through rows sharing one timestamp without skips or repeats" $ do
        (uid, allNids) <- runDB $ do
          uid <- createTestUser
          nids <- mapM (\n -> createNoteAt uid (tshow (n :: Int)) t0) [1 .. 5]
          return (uid, nids)
        -- newest-first display of same-time rows falls back to id desc
        let walk acc mcursor = do
              (nids, _, _) <- runDB $ queryDir SortDesc uid mcursor 2
              case lastMay nids of
                Nothing -> pure acc
                Just lastNid -> walk (acc <> nids) (Just (PagingCursorBefore (NoteCursor t0 (Just lastNid))))
        collected <- walk [] Nothing
        liftIO $ collected `shouldBe` reverse allNids

      it "pages forward through rows sharing one timestamp without skips or repeats" $ do
        (uid, allNids) <- runDB $ do
          uid <- createTestUser
          nids <- mapM (\n -> createNoteAt uid (tshow (n :: Int)) t0) [1 .. 5]
          return (uid, nids)
        -- oldest-first display of same-time rows falls back to id asc
        let walk acc mcursor = do
              (nids, _, _) <- runDB $ queryDir SortAsc uid mcursor 2
              case lastMay nids of
                Nothing -> pure acc
                Just lastNid -> walk (acc <> nids) (Just (PagingCursorAfter (NoteCursor t0 (Just lastNid))))
        collected <- walk [] Nothing
        liftIO $ collected `shouldBe` allNids
