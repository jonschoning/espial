module Model where

import Control.Monad.Except (runExcept)
import Data.Array (intercalate)
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable)
import Data.String (Pattern(..), split)
import Foreign (Foreign, readInt, readString, unsafeToForeign)
import Foreign.Object (Object)
import Prelude (class Eq, pure, ($), (<$>))
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
derive newtype instance J.ReadForeign Bookmark'
derive newtype instance J.WriteForeign Bookmark'

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
derive newtype instance J.ReadForeign Note'
derive newtype instance J.WriteForeign Note'

type AccountSettings =
  { archiveDefault :: Boolean
  , privateDefault :: Boolean
  , privacyLock :: Boolean
  }

newtype AccountSettings' = AccountSettings' AccountSettings
derive newtype instance J.ReadForeign AccountSettings'
derive newtype instance J.WriteForeign AccountSettings'

type TagCloudMode =
  { mode :: String
  , value :: Foreign
  , expanded :: Boolean
  }
newtype TagCloudMode' = TagCloudMode' TagCloudMode
derive newtype instance J.ReadForeign TagCloudMode'
derive newtype instance J.WriteForeign TagCloudMode'

type TagCloud = Object Int

data TagCloudModeF
  = TagCloudModeTop Boolean Int 
  | TagCloudModeLowerBound Boolean Int 
  | TagCloudModeRelated Boolean (Array String) 
  | TagCloudModeNone

derive instance Eq TagCloudModeF

tagCloudModeToF :: TagCloudMode -> TagCloudModeF
tagCloudModeToF tagCloudMode =
  fromMaybe TagCloudModeNone $ hush $ runExcept $
    case tagCloudMode.mode of
      "top" -> TagCloudModeTop tagCloudMode.expanded <$> readInt tagCloudMode.value
      "lowerBound" -> TagCloudModeLowerBound tagCloudMode.expanded <$> readInt tagCloudMode.value
      "related" -> (\s -> TagCloudModeRelated tagCloudMode.expanded (split (Pattern " ") s)) <$> readString tagCloudMode.value
      _ -> pure TagCloudModeNone

tagCloudModeFromF :: TagCloudModeF -> TagCloudMode
tagCloudModeFromF (TagCloudModeTop e i) =
  { mode: "top" , value: unsafeToForeign i, expanded: e }
tagCloudModeFromF (TagCloudModeLowerBound e i) =
  { mode: "lowerBound" , value: unsafeToForeign i, expanded: e }
tagCloudModeFromF (TagCloudModeRelated e tags) =
  { mode: "related" , value: unsafeToForeign (intercalate " " tags), expanded: e  }
tagCloudModeFromF TagCloudModeNone =
  { mode: "none" , value: unsafeToForeign "", expanded: false }
    
isExpanded :: TagCloudModeF -> Boolean
isExpanded (TagCloudModeTop e _) = e
isExpanded (TagCloudModeLowerBound e _) = e
isExpanded (TagCloudModeRelated e _) = e
isExpanded TagCloudModeNone = false

isRelated :: TagCloudModeF -> Boolean
isRelated (TagCloudModeRelated _ _) = true
isRelated _ = false

setExpanded :: TagCloudModeF -> Boolean -> TagCloudModeF
setExpanded (TagCloudModeTop _ i) e' = TagCloudModeTop e' i
setExpanded (TagCloudModeLowerBound _ i) e' = TagCloudModeLowerBound e' i
setExpanded (TagCloudModeRelated _ i) e' = TagCloudModeRelated e' i
setExpanded TagCloudModeNone _ = TagCloudModeNone

showMode :: TagCloudModeF -> String
showMode (TagCloudModeTop _ _)  = "top"
showMode (TagCloudModeLowerBound _ _)  = "lowerBound"
showMode (TagCloudModeRelated _ _)  = "related"
showMode TagCloudModeNone = ""

type TagSuggestions = 
  { query :: String
  , suggestions :: Array TSuggestion
  } 
newtype TagSuggestions' = TagSuggestions' TagSuggestions
derive newtype instance J.ReadForeign TagSuggestions'
derive newtype instance J.WriteForeign TagSuggestions'

type TSuggestion = 
  { term :: String
  , count :: Int
  } 
newtype TSuggestion' = TSuggestion' TSuggestion
derive newtype instance J.ReadForeign TSuggestion'
derive newtype instance J.WriteForeign TSuggestion'
