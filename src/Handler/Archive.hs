{-# LANGUAGE NamedFieldPuns #-}

module Handler.Archive where

import Archiver.Backend (ArchiverBackend (..))
import Handler.Common (espialUserAgent)
import Import

shouldArchiveBookmark :: User -> Key Bookmark -> Handler Bool
shouldArchiveBookmark user kbid = do
  mArchiver <- appArchiver <$> getYesod
  case mArchiver of
    Nothing -> pure False
    Just ArchiverBackend {isUrlDenylisted} ->
      runDB (get kbid) >>= \case
        Nothing -> pure False
        Just bm ->
          pure
            $ isNothing (bookmarkArchiveHref bm)
            && bookmarkShared bm
            && userArchiveDefault user
            && not (isUrlDenylisted (Url (bookmarkHref bm)))

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