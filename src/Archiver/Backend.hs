module Archiver.Backend where

import ClassyPrelude
import Database.Persist.Sql (Key, SqlBackend)
import Model (Bookmark, Url, User, UserAgent)

data ArchiverBackend = ArchiverBackend
  { runArchiver :: Key User -> Key Bookmark -> UserAgent -> Url -> IO (),
    isUrlDenylisted :: Url -> Bool
  }

-- | DB actions handed to archiver backends, which run outside the Handler monad.
data ArchiverDB = ArchiverDB
  { archiverRunDB :: forall a. ReaderT SqlBackend IO a -> IO a,
    archiverRunDBWrite :: forall a. ReaderT SqlBackend IO a -> IO a
  }
