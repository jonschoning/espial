-- | Shared plumbing for archiver backends that shell out to a local tool and write
-- the result under a per-bookmark directory served back by 'Handler.Archive'.
module Archiver.LocalArchive
  ( localArchiveDir,
    localArchiveFile,
    localArchiveHref,
    probeExe,
    runProcessQuiet,
    runProcessWithStdin,
  )
where

import ClassyPrelude
import Data.ByteString.Lazy qualified as BSL
import Database.Persist.Sql (Key, fromSqlKey)
import Model (Bookmark, User)
import System.Exit (ExitCode (..))
import System.Process
  ( CreateProcess (..),
    StdStream (CreatePipe, Inherit, NoStream),
    proc,
    readProcessWithExitCode,
    waitForProcess,
    withCreateProcess,
  )

localArchiveDir :: FilePath -> Key User -> Key Bookmark -> FilePath
localArchiveDir baseDir userId bookmarkId =
  baseDir </> show (fromSqlKey userId) </> show (fromSqlKey bookmarkId)

localArchiveFile :: FilePath -> Key User -> Key Bookmark -> FilePath
localArchiveFile baseDir userId bookmarkId =
  localArchiveDir baseDir userId bookmarkId </> archiveFileName

localArchiveHref :: Key Bookmark -> Text
localArchiveHref bookmarkId = "/archive/bm/" <> tshow (fromSqlKey bookmarkId)

archiveFileName :: FilePath
archiveFileName = "latest.html"

-- | Startup liveness check. Captures rather than inherits the streams so the tool's
-- banner doesn't land in the server log.
probeExe :: FilePath -> [String] -> IO (Maybe ExitCode)
probeExe exe args =
  timeout 10000000 $ do
    (code, _, _) <- readProcessWithExitCode exe args ""
    pure code

-- | Runs a command with no stdin, killing it if it outlives the timeout.
-- Streams are inherited rather than piped so a chatty child can't deadlock on a full pipe.
runProcessQuiet :: FilePath -> [String] -> Int -> IO (Maybe ExitCode)
runProcessQuiet exe args timeoutMicros =
  timeout timeoutMicros $
    withCreateProcess
      (proc exe args) {std_in = NoStream, std_out = Inherit, std_err = Inherit}
      (\_ _ _ ph -> waitForProcess ph)

-- | Like 'runProcessQuiet', but writes @input@ to the child's stdin before waiting on it.
runProcessWithStdin :: FilePath -> [String] -> LByteString -> Int -> IO (Maybe ExitCode)
runProcessWithStdin exe args input timeoutMicros =
  timeout timeoutMicros $
    withCreateProcess
      (proc exe args) {std_in = CreatePipe, std_out = Inherit, std_err = Inherit}
      ( \mstdin _ _ ph -> do
          forM_ mstdin $ \h -> BSL.hPut h input >> hClose h
          waitForProcess ph
      )
