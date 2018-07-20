module Model where

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
  , selected :: Boolean
  , time :: String
  }

newtype Bookmark' = Bookmark' Bookmark
derive newtype instance rfI :: J.ReadForeign Bookmark'
derive newtype instance wfI :: J.WriteForeign Bookmark'
