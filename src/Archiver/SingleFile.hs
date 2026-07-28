module Archiver.SingleFile
  ( singleFileBackend,
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

data SingleFileContext = SingleFileContext
  { singleFileExe :: FilePath,
    singleFileDir :: FilePath,
    singleFileArgs :: [String],
    singleFileTimeoutMicros :: Int,
    singleFileDB :: ArchiverDB
  }

singleFileBackend :: AppSettings -> ArchiverDB -> LogFunc -> IO (Maybe ArchiverBackend)
singleFileBackend AppSettings {..} archiverDB logFunc = flip runLoggingT logFunc $ do
  let exe = unpack (T.strip appSingleFilePath)
      browserServer = T.strip appSingleFileBrowserServer
      browserArgs
        | T.null browserServer =
            [ "--browser-executable-path=" <> unpack (T.strip appSingleFileBrowserPath),
              "--browser-args=" <> unpack (T.strip appSingleFileBrowserArgs)
            ]
        | otherwise = ["--browser-server=" <> unpack browserServer]
      ctx =
        SingleFileContext
          { singleFileExe = exe,
            singleFileDir = appSingleFileDir,
            singleFileArgs = browserArgs <> map unpack (words appSingleFileArgs),
            singleFileTimeoutMicros = appSingleFileTimeoutSec * 1000000,
            singleFileDB = archiverDB
          }
  dirOk <- liftIO $ tryAny (createDirectoryIfMissing True appSingleFileDir)
  exeOk <- liftIO $ tryAny (probeExe exe ["--version"])
  case (dirOk, exeOk) of
    (Left e, _) -> do
      $(logWarn) $ "Archive backend `singlefile` selected but output dir " <> pack appSingleFileDir <> " is not writable (" <> tshow e <> "); archiving disabled"
      pure Nothing
    (_, Left e) -> do
      $(logWarn) $ "Archive backend `singlefile` selected but " <> pack exe <> " could not be run (" <> tshow e <> "); archiving disabled"
      pure Nothing
    (_, Right Nothing) -> do
      $(logWarn) $ "Archive backend `singlefile` selected but " <> pack exe <> " --version timed out; archiving disabled"
      pure Nothing
    (_, Right (Just code)) | code /= ExitSuccess -> do
      $(logWarn) $ "Archive backend `singlefile` selected but " <> pack exe <> " --version exited with " <> tshow code <> "; archiving disabled"
      pure Nothing
    _ ->
      pure $
        Just
          ArchiverBackend
            { runArchiver = \uid bid url -> flip runLoggingT logFunc $ _singleFileRun ctx uid bid url,
              isUrlDenylisted = \(Url url) ->
                let scheme = toLower (T.takeWhile (/= ':') url)
                 in not (scheme == "http" || scheme == "https")
            }

_singleFileRun :: SingleFileContext -> Key User -> Key Bookmark -> Url -> LoggingT IO ()
_singleFileRun SingleFileContext {..} userId bookmarkId url = do
  let out = localArchiveFile singleFileDir userId bookmarkId
      tmp = out <> ".tmp"
  $(logDebug) $ "Archiving URL with single-file: " <> unUrl url
  liftIO $ createDirectoryIfMissing True (localArchiveDir singleFileDir userId bookmarkId)
  result <-
    liftIO $
      tryAny (runProcessQuiet singleFileExe (singleFileArgs <> [unpack (unUrl url), tmp]) singleFileTimeoutMicros)
  case result of
    Left e -> discardTmp tmp $ "single-file invocation failed: " <> tshow e
    Right Nothing -> discardTmp tmp $ "single-file timed out after " <> tshow (singleFileTimeoutMicros `div` 1000000) <> "s: " <> unUrl url
    Right (Just code) | code /= ExitSuccess -> discardTmp tmp $ "single-file exited with " <> tshow code <> ": " <> unUrl url
    Right _ -> do
      size <- liftIO $ tryAny (getFileSize tmp)
      case size of
        Right n | n > 0 -> do
          liftIO $ renamePath tmp out
          let href = localArchiveHref bookmarkId
          $(logDebug) $ "storing archive link: " <> href
          liftIO $ archiverRunDBWrite singleFileDB (updateBookmarkArchiveUrl userId bookmarkId (Just href))
        _ -> discardTmp tmp $ "single-file produced no output for " <> unUrl url
  where
    discardTmp tmp warning = do
      $(logWarn) warning
      liftIO $ void $ tryAny $ whenM (doesFileExist tmp) (removeFile tmp)
