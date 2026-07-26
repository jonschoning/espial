module Archiver.Monolith
  ( monolithBackend,
  )
where

import Archiver.Backend
import Archiver.LocalArchive
import ClassyPrelude
import Control.Monad.Logger (LoggingT, logDebug, logWarn, runLoggingT)
import Data.Text qualified as T
import Database.Persist.Sql (Key)
import Model (Bookmark, Url (..), User, updateBookmarkArchiveUrl)
import Settings (AppSettings (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, removeFile, renamePath)
import System.Exit (ExitCode (..))
import Yesod.Default.Main (LogFunc)

data MonolithContext = MonolithContext
  { monolithExe :: FilePath,
    monolithDir :: FilePath,
    monolithExtraArgs :: [String],
    monolithTimeoutMicros :: Int,
    monolithDB :: ArchiverDB
  }

monolithBackend :: AppSettings -> ArchiverDB -> LogFunc -> IO (Maybe ArchiverBackend)
monolithBackend AppSettings {..} archiverDB logFunc = flip runLoggingT logFunc $ do
  let exe = unpack (T.strip appMonolithPath)
      ctx =
        MonolithContext
          { monolithExe = exe,
            monolithDir = appMonolithDir,
            monolithExtraArgs = map unpack (words appMonolithArgs),
            monolithTimeoutMicros = appMonolithTimeoutSec * 1000000,
            monolithDB = archiverDB
          }
  dirOk <- liftIO $ tryAny (createDirectoryIfMissing True appMonolithDir)
  exeOk <- liftIO $ tryAny (probeExe exe ["--version"])
  case (dirOk, exeOk) of
    (Left e, _) -> do
      $(logWarn) $ "Archive backend `monolith` selected but output dir " <> pack appMonolithDir <> " is not writable (" <> tshow e <> "); archiving disabled"
      pure Nothing
    (_, Left e) -> do
      $(logWarn) $ "Archive backend `monolith` selected but " <> pack exe <> " could not be run (" <> tshow e <> "); archiving disabled"
      pure Nothing
    (_, Right Nothing) -> do
      $(logWarn) $ "Archive backend `monolith` selected but " <> pack exe <> " --version timed out; archiving disabled"
      pure Nothing
    (_, Right (Just code)) | code /= ExitSuccess -> do
      $(logWarn) $ "Archive backend `monolith` selected but " <> pack exe <> " --version exited with " <> tshow code <> "; archiving disabled"
      pure Nothing
    _ ->
      pure $
        Just
          ArchiverBackend
            { runArchiver = \uid bid url -> flip runLoggingT logFunc $ _monolithRun ctx uid bid url,
              isUrlDenylisted = \(Url url) ->
                let scheme = toLower (T.takeWhile (/= ':') url)
                 in not (scheme == "http" || scheme == "https")
            }

_monolithRun :: MonolithContext -> Key User -> Key Bookmark -> Url -> LoggingT IO ()
_monolithRun MonolithContext {..} userId bookmarkId url = do
  let out = localArchiveFile monolithDir userId bookmarkId
      tmp = out <> ".tmp"
  $(logDebug) $ "Archiving URL with monolith: " <> unUrl url
  liftIO $ createDirectoryIfMissing True (localArchiveDir monolithDir userId bookmarkId)
  result <-
    liftIO $
      tryAny (runProcessQuiet monolithExe (monolithExtraArgs <> ["-o", tmp, unpack (unUrl url)]) monolithTimeoutMicros)
  case result of
    Left e -> discardTmp tmp $ "monolith invocation failed: " <> tshow e
    Right Nothing -> discardTmp tmp $ "monolith timed out after " <> tshow (monolithTimeoutMicros `div` 1000000) <> "s: " <> unUrl url
    Right (Just code) | code /= ExitSuccess -> discardTmp tmp $ "monolith exited with " <> tshow code <> ": " <> unUrl url
    Right _ -> do
      size <- liftIO $ tryAny (getFileSize tmp)
      case size of
        Right n | n > 0 -> do
          liftIO $ renamePath tmp out
          let href = localArchiveHref bookmarkId
          $(logDebug) $ "storing archive link: " <> href
          liftIO $ archiverRunDBWrite monolithDB (updateBookmarkArchiveUrl userId bookmarkId (Just href))
        _ -> discardTmp tmp $ "monolith produced no output for " <> unUrl url
  where
    discardTmp tmp warning = do
      $(logWarn) warning
      liftIO $ void $ tryAny $ whenM (doesFileExist tmp) (removeFile tmp)
