module Model where

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
  , selected :: Boolean
  , time :: String
  }
