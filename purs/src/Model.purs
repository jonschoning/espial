module Model where

import Control.Monad
import Foreign
import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (intercalate, singleton)
import Data.Either (Either, hush)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable)
import Foreign.Object (Object)
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

type TagCloudMode =
  { mode :: String
  , value :: Foreign
  , expanded :: Boolean
  }
newtype TagCloudMode' = TagCloudMode' TagCloudMode
derive newtype instance tagcloudmode_rfi :: J.ReadForeign TagCloudMode'
derive newtype instance tagcloudmode_wfI :: J.WriteForeign TagCloudMode'

type TagCloud = Object Int

data TagCloudModeF
  = TagCloudModeTop Boolean Int 
  | TagCloudModeLowerBound Boolean Int 
  | TagCloudModeRelated Boolean (Array String) 
  | TagCloudModeNone

derive instance eqTagCloudModeF :: Eq TagCloudModeF

tagCloudModeToF :: TagCloudMode -> TagCloudModeF
tagCloudModeToF tagCloudMode =
  fromMaybe TagCloudModeNone $ hush $ runExcept $
    case tagCloudMode.mode of
      "top" -> TagCloudModeTop tagCloudMode.expanded <$> readInt tagCloudMode.value
      "lowerBound" -> TagCloudModeLowerBound tagCloudMode.expanded <$> readInt tagCloudMode.value
      "related" -> (\s -> TagCloudModeRelated tagCloudMode.expanded (singleton s)) <$> readString tagCloudMode.value
      _ -> pure TagCloudModeNone

tagCloudModeFromF :: TagCloudModeF -> TagCloudMode
tagCloudModeFromF (TagCloudModeTop e i) =
  { mode: "top" , value: unsafeToForeign i, expanded: e }
tagCloudModeFromF (TagCloudModeLowerBound e i) =
  { mode: "lowerBound" , value: unsafeToForeign i, expanded: e }
tagCloudModeFromF (TagCloudModeRelated e tags) =
  { mode: "related" , value: unsafeToForeign (intercalate " " tags), expanded: e  }
tagCloudModeFromF TagCloudModeNone =
  { mode: "related" , value: unsafeToForeign "", expanded: false }
    
isExpanded :: TagCloudModeF -> Boolean
isExpanded (TagCloudModeTop e _) = e
isExpanded (TagCloudModeLowerBound e _) = e
isExpanded (TagCloudModeRelated e _) = e
isExpanded TagCloudModeNone = false

setExpanded :: TagCloudModeF -> Boolean -> TagCloudModeF
setExpanded (TagCloudModeTop e i) e' = TagCloudModeTop e' i
setExpanded (TagCloudModeLowerBound e i) e' = TagCloudModeLowerBound e' i
setExpanded (TagCloudModeRelated e i) e' = TagCloudModeRelated e' i
setExpanded TagCloudModeNone _ = TagCloudModeNone

isSameMode :: TagCloudModeF -> TagCloudModeF -> Boolean
isSameMode (TagCloudModeTop _ _) (TagCloudModeTop _ _) = true
isSameMode (TagCloudModeLowerBound _ _) (TagCloudModeLowerBound _ _) = true
isSameMode (TagCloudModeRelated _ _) (TagCloudModeRelated _ _) = true
isSameMode TagCloudModeNone TagCloudModeNone = true
isSameMode _ _ = false

showMode :: TagCloudModeF -> String
showMode (TagCloudModeTop _ _)  = "top"
showMode (TagCloudModeLowerBound _ _)  = "lowerBound"
showMode (TagCloudModeRelated _ _)  = "related"
showMode TagCloudModeNone = ""
