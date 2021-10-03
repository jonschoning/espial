{-# OPTIONS_GHC -fno-warn-orphans #-}

module PathPiece where
  
import Data.Text (breakOn, splitOn)
import qualified Data.Text as T (replace)
import Import.NoFoundation

-- PathPiece

instance PathPiece UserNameP where
  toPathPiece (UserNameP i) = "u:" <> i
  fromPathPiece s =
    case breakOn ":" s of
      ("u", "") -> Nothing
      ("u", uname) -> Just $ UserNameP (drop 1 uname)
      _ -> Nothing

instance PathPiece TagsP where
  toPathPiece (TagsP tags) = "t:" <> intercalate "+" (fmap encodeTag tags)
  fromPathPiece s =
    case breakOn ":" s of
      ("t", "") -> Nothing
      ("t", tags) -> Just $ (TagsP . fmap decodeTag . splitOn "+" . drop 1) tags
      _ -> Nothing

encodeTag :: Text -> Text
encodeTag = T.replace "+" "%2B"

decodeTag :: Text -> Text
decodeTag = T.replace "%2B" "+"

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
    FilterSingle slug -> "b:" <> unBmSlug slug
  fromPathPiece = \case
    "unread" -> Just FilterUnread
    "untagged" -> Just FilterUntagged
    "starred" -> Just FilterStarred
    s -> case breakOn ":" s of
        ("b", "") -> Nothing
        ("b", slug) -> Just $ FilterSingle (BmSlug (drop 1 slug))
        _ -> Nothing


deriving instance PathPiece NtSlug 
deriving instance PathPiece BmSlug 
