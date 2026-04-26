module Archiver.Backend where

import ClassyPrelude
import Database.Persist.Sql (Key)
import Model (Bookmark, Url, User, UserAgent)

data ArchiverBackend = ArchiverBackend
  { runArchiver :: Key User -> Key Bookmark -> UserAgent -> Url -> IO (),
    isUrlDenylisted :: Url -> Bool
  }
