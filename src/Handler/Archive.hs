module Handler.Archive where

import Archiver.Backend (ArchiveJob (..), ArchiverBackend (..), enqueueArchiveJobs)
import Archiver.LocalArchive (localArchiveDir, localArchiveFile)
import Handler.Locales (isWithinDir)
import Import
import Network.PrivateAddress (isDisallowedFetchUrl)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

postArchiveBookmarkR :: Int64 -> Handler ()
postArchiveBookmarkR bid = do
  let kbid = toSqlKey bid
  (userId, _) <- requireAuthPair
  runDB (get kbid) >>= \case
    Just bm | (bookmarkUserId bm == userId) -> archiveBookmarkUrl kbid bm
    _ -> notFound

-- | Serves an on-disk archive. Owner-only, matching the fact that `archiveHref` is
-- stripped from bookmarks shown to anyone else (see `Model.Form`).
getArchiveFileR :: Int64 -> Handler ()
getArchiveFileR bid = do
  let kbid = toSqlKey bid
  (userId, _) <- requireAuthPair
  runDB (get kbid) >>= \case
    Just bm | bookmarkUserId bm == userId -> do
      baseDir <- maybe notFound pure . localArchiveBaseDir . appSettings =<< getYesod
      let path = localArchiveFile baseDir userId kbid
      unlessM (liftIO (isWithinDir baseDir path)) notFound
      -- opaque origin + no scripts: archived third-party HTML must not reach espial's session
      addHeader "Content-Security-Policy" archiveContentSecurityPolicy
      addHeader "X-Content-Type-Options" "nosniff"
      addHeader "Referrer-Policy" "no-referrer"
      sendFile typeHtml path
    _ -> notFound

-- | Best-effort removal of a bookmark's on-disk archive; the DB row is the index, so a
-- leftover directory would never be reachable again. No-op unless a backend that writes
-- local archives (monolith, singlefile, chromium) is active, or when
-- @archive-delete-local-files-on-delete@ is off.
deleteLocalBookmarkArchiveFiles :: Key User -> Key Bookmark -> Handler ()
deleteLocalBookmarkArchiveFiles userId kbid = do
  settings <- appSettings <$> getYesod
  when (appArchiveDeleteLocalFilesOnDelete settings) $ forM_ (localArchiveBaseDir settings) $ \baseDir -> do
    let dir = localArchiveDir baseDir userId kbid
    liftIO (tryAny (whenM (doesDirectoryExist dir) (removeDirectoryRecursive dir))) >>= \case
      Left e -> $(logWarn) $ "Failed to remove archive dir " <> pack dir <> ": " <> tshow e
      Right () -> pure ()

archiveContentSecurityPolicy :: Text
archiveContentSecurityPolicy =
  intercalate
    "; "
    [ -- allow-scripts without allow-same-origin: the page's own JS runs, but in an
      -- opaque origin with no access to espial's cookies or storage
      "sandbox allow-scripts allow-popups allow-popups-to-escape-sandbox",
      "default-src 'none'",
      "script-src 'unsafe-inline' 'unsafe-eval' data: blob:",
      "style-src 'unsafe-inline' data:",
      "img-src data: blob:",
      "media-src data: blob:",
      "font-src data:",
      "frame-src data: blob:",
      "connect-src 'none'",
      "form-action 'none'"
    ]

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
