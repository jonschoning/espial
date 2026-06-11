module Model.Form where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import qualified ClassyPrelude.Yesod as CP
import qualified Data.Aeson as A
import Database.Esqueleto.Experimental hiding ((<&>))
import Model
import Model.Custom
import Types

-- * AccountSettingsForm

data AccountSettingsForm = AccountSettingsForm
  { _privateDefault :: Bool,
    _archiveDefault :: Bool,
    _suggestTags :: Bool,
    _privacyLock :: Bool,
    _archiveBackendEnabled :: Bool
  }
  deriving (Show, Eq, Read, Generic)

instance FromJSON AccountSettingsForm where parseJSON = A.genericParseJSON gDefaultFormOptions

instance ToJSON AccountSettingsForm where toJSON = A.genericToJSON gDefaultFormOptions

toAccountSettingsForm :: Bool -> User -> AccountSettingsForm
toAccountSettingsForm archiveBackendEnabled User {..} =
  AccountSettingsForm
    { _privateDefault = userPrivateDefault,
      _archiveDefault = userArchiveDefault,
      _suggestTags = userSuggestTags,
      _privacyLock = userPrivacyLock,
      _archiveBackendEnabled = archiveBackendEnabled
    }

updateUserFromAccountSettingsForm :: Key User -> AccountSettingsForm -> DB ()
updateUserFromAccountSettingsForm userId AccountSettingsForm {..} =
  CP.update
    userId
    [ UserPrivateDefault CP.=. _privateDefault,
      UserArchiveDefault CP.=. _archiveDefault,
      UserSuggestTags CP.=. _suggestTags,
      UserPrivacyLock CP.=. _privacyLock
    ]

-- * BookmarkForm

data BookmarkForm = BookmarkForm
  { _url :: Text,
    _title :: Maybe Text,
    _description :: Maybe Textarea,
    _tags :: Maybe Text,
    _private :: Maybe Bool,
    _toread :: Maybe Bool,
    _bid :: Maybe Int64,
    _slug :: Maybe BmSlug,
    _selected :: Maybe Bool,
    _time :: Maybe UTCTimeStr,
    _archiveUrl :: Maybe Text,
    _archiveRequested :: Maybe Bool
  }
  deriving (Show, Eq, Read, Generic)

instance FromJSON BookmarkForm where parseJSON = A.genericParseJSON gDefaultFormOptions

instance ToJSON BookmarkForm where
  toJSON BookmarkForm {..} =
    A.object $
      [ "url" .= _url,
        "title" .= _title,
        "description" .= _description,
        "tags" .= _tags,
        "private" .= _private,
        "toread" .= _toread,
        "bid" .= _bid,
        "slug" .= _slug,
        "selected" .= _selected,
        "time" .= _time
      ]
      ++ maybe [] (\x -> ["archiveUrl" .= x]) _archiveUrl
      ++ maybe [] (\x -> ["archiveRequested" .= x]) _archiveRequested
  toEncoding BookmarkForm {..} =
    A.pairs
      $ mconcat
        [ "url" .= _url,
          "title" .= _title,
          "description" .= _description,
          "tags" .= _tags,
          "private" .= _private,
          "toread" .= _toread,
          "bid" .= _bid,
          "slug" .= _slug,
          "selected" .= _selected,
          "time" .= _time
        ]
      <> maybe mempty ("archiveUrl" .=) _archiveUrl
      <> maybe mempty ("archiveRequested" .=) _archiveRequested

gDefaultFormOptions :: A.Options
gDefaultFormOptions = A.defaultOptions {A.fieldLabelModifier = drop 1}

toBookmarkFormListForViewer :: Bool -> [(Entity Bookmark, Maybe Text)] -> [BookmarkForm]
toBookmarkFormListForViewer isowner =
  fmap $ \entry ->
    let form = _toBookmarkForm' entry
     in if isowner then form else form {_selected = Just False, _toread = Just False, _archiveUrl = Nothing}

mkNewBookmarkForm :: Bool -> User -> Text -> Maybe Text -> Maybe Textarea -> Maybe Text -> Maybe Bool -> Maybe Bool -> BookmarkForm
mkNewBookmarkForm archiveBackendEnabled user url title description tags private toread =
  BookmarkForm
    { _url = url,
      _title = title,
      _description = description,
      _tags = tags,
      _private = private,
      _toread = toread,
      _bid = Nothing,
      _slug = Nothing,
      _selected = Nothing,
      _time = Nothing,
      _archiveUrl = Nothing,
      _archiveRequested =
        if archiveBackendEnabled
          then Just (userArchiveDefault user && maybe True not private)
          else Nothing
    }

toBookmarkForm :: (Entity Bookmark, [Entity BookmarkTag]) -> BookmarkForm
toBookmarkForm (bm, tags) =
  _toBookmarkForm' (bm, Just $ tagsToText tags)
  where
    tagsToText :: [Entity BookmarkTag] -> Text
    tagsToText = unwords . fmap (bookmarkTagTag . entityVal)

_toBookmarkForm' :: (Entity Bookmark, Maybe Text) -> BookmarkForm
_toBookmarkForm' (Entity bid Bookmark {..}, tags) =
  BookmarkForm
    { _url = bookmarkHref,
      _title = Just bookmarkDescription,
      _description = Just $ Textarea $ bookmarkExtended,
      _tags = Just $ fromMaybe "" tags,
      _private = Just $ not bookmarkShared,
      _toread = Just bookmarkToRead,
      _bid = Just $ fromSqlKey $ bid,
      _slug = Just bookmarkSlug,
      _selected = Just bookmarkSelected,
      _time = Just $ UTCTimeStr $ bookmarkTime,
      _archiveUrl = bookmarkArchiveHref,
      _archiveRequested = Nothing
    }

bookmarkFormToBookmark :: UserId -> BookmarkForm -> IO Bookmark
bookmarkFormToBookmark userId BookmarkForm {..} = do
  time <- liftIO getCurrentTime
  slug <- maybe mkBmSlug pure _slug
  pure
    $ Bookmark
      { bookmarkUserId = userId,
        bookmarkSlug = slug,
        bookmarkHref = _url,
        bookmarkDescription = fromMaybe "" _title,
        bookmarkExtended = maybe "" unTextarea _description,
        bookmarkTime = maybe time unUTCTimeStr _time,
        bookmarkShared = maybe True not _private,
        bookmarkToRead = Just True == _toread,
        bookmarkSelected = Just True == _selected,
        bookmarkArchiveHref = _archiveUrl
      }

-- * Tag suggestions

data TagSuggestionRequest = TagSuggestionRequest
  { _query :: Text,
    _currentTags :: Maybe [Text]
  }
  deriving (Show, Eq, Read, Generic)

data TagSuggestionResponse = TagSuggestionResponse
  { _suggestions :: [Suggestion]
  }
  deriving (Show, Eq, Read, Generic)

data Suggestion = Suggestion
  { _term :: Text,
    _count :: Int
  }
  deriving (Show, Eq, Read, Generic)

instance FromJSON TagSuggestionRequest where parseJSON = A.genericParseJSON gDefaultFormOptions

instance ToJSON TagSuggestionRequest where toJSON = A.genericToJSON gDefaultFormOptions

instance FromJSON TagSuggestionResponse where parseJSON = A.genericParseJSON gDefaultFormOptions

instance ToJSON TagSuggestionResponse where toJSON = A.genericToJSON gDefaultFormOptions

instance FromJSON Suggestion where parseJSON = A.genericParseJSON gDefaultFormOptions

instance ToJSON Suggestion where toJSON = A.genericToJSON gDefaultFormOptions

getTagSuggestions :: Key User -> TagSuggestionRequest -> Int -> DB TagSuggestionResponse
getTagSuggestions user TagSuggestionRequest {_query = query, _currentTags = currentTags} top = do
  values <-
    fmap (bimap unValue unValue)
      <$> ( select $ do
              (tag, countRows', matchPriority) <- from suggestionsQuery
              orderBy [asc matchPriority, desc countRows', asc $ lower_ tag]
              limit (fromIntegral top)
              pure (tag, countRows')
          )
  pure
    TagSuggestionResponse
      { _suggestions = uncurry Suggestion <$> values
      }
  where
    excludedTags = take 400 $ map toLower (fromMaybe ([] :: [Text]) currentTags)

    suggestionsQuery = do
      t <- from (table @BookmarkTag)
      where_
        ( t
            ^. BookmarkTagUserId
            ==. val user
            &&. (lower_ (t ^. BookmarkTagTag) `like` lower_ ((%) ++. val query ++. (%)))
        )
      unless (null excludedTags) $ where_ $ not_ (lower_ (t ^. BookmarkTagTag) `in_` valList excludedTags)
      let countRows' = countRows
          matchPriority =
            case_
              [ when_ (lower_ (t ^. BookmarkTagTag) ==. lower_ (val query)) then_ (val (0 :: Int)),
                when_ (lower_ (t ^. BookmarkTagTag) `like` lower_ (val query ++. (%))) then_ (val (1 :: Int))
              ]
              (else_ (val (2 :: Int)))
      groupBy (lower_ (t ^. BookmarkTagTag))
      pure (t ^. BookmarkTagTag, countRows', matchPriority)
