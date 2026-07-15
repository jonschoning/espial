module Handler.Archive where

import Archiver.Backend (ArchiveJob (..), ArchiverBackend (..), enqueueArchiveJobs)
import Import

postArchiveBookmarkR :: Int64 -> Handler ()
postArchiveBookmarkR bid = do
  let kbid = toSqlKey bid
  (userId, _) <- requireAuthPair
  runDB (get kbid) >>= \case
    Just bm
      | (bookmarkUserId bm == userId) ->
          whenM (shouldArchiveBookmark bm)
            $ archiveBookmarkUrl kbid (Url (bookmarkHref bm))
    _ -> notFound

archiveBookmarkUrl :: Key Bookmark -> Url -> Handler ()
archiveBookmarkUrl kbid url = archiveBookmarkUrls [(kbid, url)]

archiveBookmarkUrls :: [(Key Bookmark, Url)] -> Handler ()
archiveBookmarkUrls kbidUrls = do
  app <- getYesod
  case appArchiver app of
    Just (ArchiverBackend {isUrlDenylisted}, queue) -> do
      userId <- requireAuthId
      let jobs = [ArchiveJob userId kbid url | (kbid, url) <- kbidUrls, not (isUrlDenylisted url)]
      unless (null jobs)
        $ void (enqueueArchiveJobs queue jobs)
          `catch` (\(e :: SomeException) -> $(logError) ("Failed to enqueue archive jobs for bookmarks " <> tshow (map fst kbidUrls) <> ": " <> tshow e))
    _ -> pure ()

shouldArchiveBookmark :: Bookmark -> Handler Bool
shouldArchiveBookmark bm = do
  b <- runMaybeT $ do
    (ArchiverBackend {isUrlDenylisted}, _) <- MaybeT (appArchiver <$> getYesod)
    guard (bookmarkShared bm)
    guard (not (isUrlDenylisted (Url (bookmarkHref bm))))
  pure (isJust b)
