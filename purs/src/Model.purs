module Model where

import Data.Nullable (Nullable)
import Simple.JSON as J

type BookmarkId = Int
type TagId = Int

type Bookmark =
  { url :: String
  , title :: String
  , description :: String
  , tags :: String
  , private :: Boolean
  , toread :: Boolean
  , bid :: BookmarkId
  , slug :: String
  , selected :: Boolean
  , time :: String
  , archiveUrl :: Nullable String
  }

newtype Bookmark' = Bookmark' Bookmark
derive newtype instance bookmark_rfI :: J.ReadForeign Bookmark'
derive newtype instance bookmark_wfI :: J.WriteForeign Bookmark'

type NoteId = Int
type NoteSlug = String

type Note =
  { id :: NoteId
  , slug :: NoteSlug
  , title :: String
  , text :: String
  , length :: Int
  , isMarkdown :: Boolean
  , shared :: Boolean
  , created :: String
  , updated :: String
  }

newtype Note' = Note' Note
derive newtype instance note_rfI :: J.ReadForeign Note'
derive newtype instance note_wfI :: J.WriteForeign Note'

type AccountSettings =
  { archiveDefault :: Boolean
  , privateDefault :: Boolean
  , privacyLock :: Boolean
  }

newtype AccountSettings' = AccountSettings' AccountSettings
derive newtype instance usersettings_rfI :: J.ReadForeign AccountSettings'
derive newtype instance usersettings_wfI :: J.WriteForeign AccountSettings'
