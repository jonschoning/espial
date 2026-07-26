module Archiver.Monolith
  ( monolithBackend,
    monolithArchiveDir,
    monolithArchiveFile,
    monolithArchiveHref,
  )
where

import Archiver.Backend
import ClassyPrelude
import Control.Monad.Logger (LoggingT, logDebug, logWarn, runLoggingT)
import Data.Text qualified as T
import Database.Persist.Sql (Key, fromSqlKey)
import Model (Bookmark, Url (..), User, updateBookmarkArchiveUrl)
import Settings (AppSettings (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize, removeFile, renamePath)
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (..),
    StdStream (Inherit, NoStream),
    proc,
    readProcessWithExitCode,
    waitForProcess,
    withCreateProcess,
  )
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
  exeOk <- liftIO $ tryAny (probeExe exe)
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
  let dir = monolithArchiveDir monolithDir userId bookmarkId
      out = dir </> archiveFileName
      tmp = out <> ".tmp"
  $(logDebug) $ "Archiving URL with monolith: " <> unUrl url
  liftIO $ createDirectoryIfMissing True dir
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
          let href = monolithArchiveHref bookmarkId
          $(logDebug) $ "storing archive link: " <> href
          liftIO $ archiverRunDBWrite monolithDB (updateBookmarkArchiveUrl userId bookmarkId (Just href))
        _ -> discardTmp tmp $ "monolith produced no output for " <> unUrl url
  where
    discardTmp tmp warning = do
      $(logWarn) warning
      liftIO $ void $ tryAny $ whenM (doesFileExist tmp) (removeFile tmp)

-- | Startup liveness check. Captures rather than inherits the streams so monolith's
-- banner doesn't land in the server log.
probeExe :: FilePath -> IO (Maybe ExitCode)
probeExe exe =
  timeout 10000000 $ do
    (code, _, _) <- readProcessWithExitCode exe ["--version"] ""
    pure code

-- | Runs a command with no stdin, killing it if it outlives the timeout.
-- Streams are inherited rather than piped so a chatty child can't deadlock on a full pipe.
runProcessQuiet :: FilePath -> [String] -> Int -> IO (Maybe ExitCode)
runProcessQuiet exe args timeoutMicros =
  timeout timeoutMicros $
    withCreateProcess
      (proc exe args) {std_in = NoStream, std_out = Inherit, std_err = Inherit}
      (\_ _ _ ph -> waitForProcess ph)

monolithArchiveDir :: FilePath -> Key User -> Key Bookmark -> FilePath
monolithArchiveDir baseDir userId bookmarkId =
  baseDir </> show (fromSqlKey userId) </> show (fromSqlKey bookmarkId)

monolithArchiveFile :: FilePath -> Key User -> Key Bookmark -> FilePath
monolithArchiveFile baseDir userId bookmarkId =
  monolithArchiveDir baseDir userId bookmarkId </> archiveFileName

monolithArchiveHref :: Key Bookmark -> Text
monolithArchiveHref bookmarkId = "/archive/bm/" <> tshow (fromSqlKey bookmarkId)

archiveFileName :: FilePath
archiveFileName = "latest.html"
