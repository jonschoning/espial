{-# OPTIONS_GHC -fno-warn-orphans #-}

module PathPiece where
  
import Data.Text (splitOn)

import Import.NoFoundation

-- PathPiece

instance PathPiece UserNameP where
  toPathPiece (UserNameP i) = "u:" <> i
  fromPathPiece s =
    case splitOn ":" s of
      ["u", ""] -> Nothing
      ["u", uname] -> Just $ UserNameP uname
      _ -> Nothing

instance PathPiece TagsP where
  toPathPiece (TagsP tags) = "t:" <> (intercalate "+" tags)
  fromPathPiece s =
    case splitOn ":" s of
      ["t", ""] -> Nothing
      ["t", tags] -> Just $ TagsP (splitOn "+" tags)
      _ -> Nothing

instance PathPiece SharedP where
  toPathPiece = \case
    SharedAll -> ""
    SharedPublic -> "public"
    SharedPrivate -> "private"
  fromPathPiece = \case
    "public" -> Just SharedPublic
    "private" -> Just SharedPrivate
    _ -> Nothing

instance PathPiece FilterP where
  toPathPiece = \case
    FilterAll -> ""
    FilterUnread -> "unread"
    FilterUntagged -> "untagged"
    FilterStarred -> "starred"
    FilterSingle bid -> "b:" <> (pack . show) bid
  fromPathPiece = \case
    "unread" -> Just FilterUnread
    "untagged" -> Just FilterUntagged
    "starred" -> Just FilterStarred
    s -> case splitOn ":" s of
        ["b", ""] -> Nothing
        ["b", sbid] ->
          case readMay sbid of
            Just bid -> Just $ FilterSingle bid
            _ -> Nothing
        _ -> Nothing
