{-# LANGUAGE NamedFieldPuns #-}

module Handler.Archive where

import Archiver.Backend (ArchiverBackend (..))
import Handler.Common (espialUserAgent)
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
archiveBookmarkUrl kbid url = do
  mArchiver <- appArchiver <$> getYesod
  case mArchiver of
    Nothing -> pure ()
    Just ArchiverBackend {runArchiver, isUrlDenylisted}
      | isUrlDenylisted url -> pure ()
      | otherwise ->
          ( do
              ua <- espialUserAgent
              userId <- requireAuthId
              liftIO $ runArchiver userId kbid ua url
          )
            `catch` (\(e :: SomeException) -> ($(logError) $ (pack . show) e) >> throwIO e)

shouldArchiveBookmark :: Bookmark -> Handler Bool
shouldArchiveBookmark bm = do
  b <- runMaybeT $ do
    ArchiverBackend {isUrlDenylisted} <- MaybeT (appArchiver <$> getYesod)
    guard (bookmarkShared bm)
    guard (not (isUrlDenylisted (Url (bookmarkHref bm))))
  pure (isJust b)
