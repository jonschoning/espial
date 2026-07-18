module Archiver.Backend where

import ClassyPrelude
import Control.Concurrent (threadDelay)
import Database.Persist.Sql (Key, SqlBackend)
import Model (ArchiveJobRecord, Bookmark, Url (..), User)

data ArchiverBackend = ArchiverBackend
  { runArchiver :: Key User -> Key Bookmark -> Url -> IO (),
    isUrlDenylisted :: Url -> Bool
  }

-- | DB actions handed to archiver backends, which run outside the Handler monad.
data ArchiverDB = ArchiverDB
  { archiverRunDB :: forall a. ReaderT SqlBackend IO a -> IO a,
    archiverRunDBWrite :: forall a. ReaderT SqlBackend IO a -> IO a
  }

-- | A single archive request, queued for a background worker to run through 'runArchiver'.
data ArchiveJob = ArchiveJob (Key User) (Key Bookmark) Url

data QueuedArchiveJob = QueuedArchiveJob (Key ArchiveJobRecord) ArchiveJob

-- | durable storage for archive jobs
data ArchiveJobStore = ArchiveJobStore
  { archiveJobStoreInsertMany :: [(Key User, Key Bookmark, Url)] -> IO [Key ArchiveJobRecord],
    archiveJobStoreDelete :: Key ArchiveJobRecord -> IO (),
    archiveJobStoreLoadAll :: IO [(Key ArchiveJobRecord, ArchiveJob)],
    archiveJobStoreBookmarkExists :: Key Bookmark -> IO Bool
  }

data ArchiveQueue = ArchiveQueue (TBQueue QueuedArchiveJob) Int ArchiveJobStore

newArchiveQueue :: (MonadIO m) => ArchiveJobStore -> Int -> m ArchiveQueue
newArchiveQueue store capacity = liftIO do
  persisted <- archiveJobStoreLoadAll store
  let realCapacity = max capacity (length persisted)
  tbqueue <- newTBQueueIO (fromIntegral realCapacity)
  atomically (traverse_ (writeTBQueue tbqueue . uncurry QueuedArchiveJob) persisted)
  pure (ArchiveQueue tbqueue realCapacity store)

enqueueArchiveJob :: (MonadIO m) => ArchiveQueue -> ArchiveJob -> m Bool
enqueueArchiveJob queue job = maybe False id . listToMaybe <$> enqueueArchiveJobs queue [job]

enqueueArchiveJobs :: (MonadIO m) => ArchiveQueue -> [ArchiveJob] -> m [Bool]
enqueueArchiveJobs (ArchiveQueue tbqueue capacity store) jobs = liftIO do
  room <- atomically do
    len <- lengthTBQueue tbqueue
    pure (max 0 (capacity - fromIntegral len))
  let (fitJobs, overflowJobs) = splitAt room jobs
  storeIds <- archiveJobStoreInsertMany store [(userId, bookmarkId, url) | ArchiveJob userId bookmarkId url <- fitJobs]
  accepted <- atomically $ forM (zip storeIds fitJobs) $ \(storeId, job) -> do
    full <- isFullTBQueue tbqueue
    unless full (writeTBQueue tbqueue (QueuedArchiveJob storeId job))
    pure (not full)
  forM_ (zip storeIds accepted) $ \(storeId, ok) -> unless ok (archiveJobStoreDelete store storeId)
  pure (accepted <> (False <$ overflowJobs))

runArchiveQueueWorker :: ArchiverBackend -> ArchiveQueue -> Int -> (SomeException -> IO ()) -> IO ()
runArchiveQueueWorker archiver@ArchiverBackend {runArchiver} archiveQueue@(ArchiveQueue tbqueue _capacity store) rateLimitMicros onError =
  processJobs `catch` \e -> do
    onError e `catch` \(_ :: SomeException) -> pure ()
    threadDelay 1_000_000
    runArchiveQueueWorker archiver archiveQueue rateLimitMicros onError
  where
    processJobs = forever do
      QueuedArchiveJob storeId (ArchiveJob userId bookmarkId url) <- atomically (readTBQueue tbqueue)
      -- delete before running: at-most-once, so a crashing/poisoned URL isn't retried forever
      archiveJobStoreDelete store storeId
      whenM (archiveJobStoreBookmarkExists store bookmarkId) do
        runArchiver userId bookmarkId url `catch` onError
        threadDelay rateLimitMicros
