module Handler.Archive where

import Archiver.Backend (ArchiveJob (..), ArchiverBackend (..), enqueueArchiveJobs)
import Import
import Network.PrivateAddress (isDisallowedFetchUrl)

postArchiveBookmarkR :: Int64 -> Handler ()
postArchiveBookmarkR bid = do
  let kbid = toSqlKey bid
  (userId, _) <- requireAuthPair
  runDB (get kbid) >>= \case
    Just bm | (bookmarkUserId bm == userId) -> archiveBookmarkUrl kbid bm
    _ -> notFound

archiveBookmarkUrl :: Key Bookmark -> Bookmark -> Handler ()
archiveBookmarkUrl kbid bm = archiveBookmarkUrls [(kbid, bm)]

archiveBookmarkUrls :: [(Key Bookmark, Bookmark)] -> Handler ()
archiveBookmarkUrls kbidBms = do
  app <- getYesod
  case appArchiver app of
    Just (_, queue) -> do
      userId <- requireAuthId
      jobs <- fmap catMaybes $ forM kbidBms $ \(kbid, bm) -> do
        should <- shouldArchiveBookmark bm
        pure (if should then Just (ArchiveJob userId kbid (Url (bookmarkHref bm))) else Nothing)
      unless (null jobs)
        $ void (enqueueArchiveJobs queue jobs)
          `catch` (\(e :: SomeException) -> $(logError) ("Failed to enqueue archive jobs for bookmarks " <> tshow (map fst kbidBms) <> ": " <> tshow e))
    _ -> pure ()

shouldArchiveBookmark :: Bookmark -> Handler Bool
shouldArchiveBookmark bm = do
  b <- runMaybeT $ do
    (ArchiverBackend {isUrlDenylisted}, _) <- MaybeT (appArchiver <$> getYesod)
    guard (bookmarkShared bm)
    guard (not (isUrlDenylisted (Url (bookmarkHref bm))))
    guard . not =<< liftIO (isDisallowedFetchUrl (bookmarkHref bm))
  pure (isJust b)
