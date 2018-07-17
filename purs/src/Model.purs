module Model where

type BookmarkId = Int
type TagId = Int

type Bookmark =
  { id :: BookmarkId
  , userId :: Int
  , href :: String
  , description :: String
  , extended :: String
  , time :: String
  , shared :: Boolean
  , toRead :: Boolean
  , selected :: Boolean
  }
