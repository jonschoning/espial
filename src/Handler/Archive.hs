{-# LANGUAGE NamedFieldPuns #-}

module Handler.Archive where

import Archiver.Backend (ArchiverBackend (..))
import Handler.Common (espialUserAgent)
import Import

shouldArchiveBookmark :: User -> Key Bookmark -> Handler Bool
shouldArchiveBookmark user kbid = do
  b <- runMaybeT $ do
    ArchiverBackend {isUrlDenylisted} <- MaybeT (appArchiver <$> getYesod)
    bm <- MaybeT (runDB (get kbid))
    guard (isNothing (bookmarkArchiveHref bm))
    guard (bookmarkShared bm)
    guard (userArchiveDefault user)
    guard (not (isUrlDenylisted (Url (bookmarkHref bm))))
  pure (isJust b)

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