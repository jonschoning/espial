{-# LANGUAGE DeriveGeneric #-}

module Model.FileBookmark where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (parseFail)
import Model
import Model.Custom
import Types

-- * FileBookmarks

data FileBookmark = FileBookmark
  { fileBookmarkHref :: !Text,
    fileBookmarkDescription :: !Text,
    fileBookmarkExtended :: !Text,
    fileBookmarkTime :: !UTCTime,
    fileBookmarkShared :: !Bool,
    fileBookmarkToRead :: !Bool,
    fileBookmarkSelected :: !(Maybe Bool),
    fileBookmarkArchiveHref :: !(Maybe Text),
    fileBookmarkTags :: !Text
  }
  deriving (Show, Eq, Typeable, Ord)

instance FromJSON FileBookmark where
  parseJSON (Object o) =
    FileBookmark
      <$> o
      .: "href"
      <*> (parseDescriptionValue =<< o .: "description")
      <*> o
      .: "extended"
      <*> o
      .: "time"
      <*> (boolFromYesNo <$> o .: "shared")
      <*> (boolFromYesNo <$> o .: "toread")
      <*> (o A..:? "selected")
      <*> (o A..:? "archive_url")
      <*> (o .: "tags")
    where
      parseDescriptionValue (String t) = pure t
      parseDescriptionValue (Bool _) = pure ""
      parseDescriptionValue _ = A.parseFail "bad parse"
  parseJSON _ = A.parseFail "bad parse"

instance ToJSON FileBookmark where
  toJSON FileBookmark {..} =
    object
      [ "href" .= toJSON fileBookmarkHref,
        "description" .= toJSON fileBookmarkDescription,
        "extended" .= toJSON fileBookmarkExtended,
        "time" .= toJSON fileBookmarkTime,
        "shared" .= toJSON (boolToYesNo fileBookmarkShared),
        "toread" .= toJSON (boolToYesNo fileBookmarkToRead),
        "selected" .= toJSON fileBookmarkSelected,
        "archive_url" .= toJSON fileBookmarkArchiveHref,
        "tags" .= toJSON fileBookmarkTags
      ]

boolFromYesNo :: Text -> Bool
boolFromYesNo "yes" = True
boolFromYesNo _ = False

boolToYesNo :: Bool -> Text
boolToYesNo True = "yes"
boolToYesNo _ = "no"

fileBookmarkToBookmark :: UserId -> FileBookmark -> IO Bookmark
fileBookmarkToBookmark user FileBookmark {..} = do
  slug <- mkBmSlug
  pure
    $ Bookmark
      { bookmarkUserId = user,
        bookmarkSlug = slug,
        bookmarkHref = fileBookmarkHref,
        bookmarkDescription = fileBookmarkDescription,
        bookmarkExtended = fileBookmarkExtended,
        bookmarkTime = fileBookmarkTime,
        bookmarkShared = fileBookmarkShared,
        bookmarkToRead = fileBookmarkToRead,
        bookmarkSelected = Just True == fileBookmarkSelected,
        bookmarkArchiveHref = fileBookmarkArchiveHref
      }

bookmarkTofileBookmark :: Bookmark -> Text -> FileBookmark
bookmarkTofileBookmark Bookmark {..} tags =
  FileBookmark
    { fileBookmarkHref = bookmarkHref,
      fileBookmarkDescription = bookmarkDescription,
      fileBookmarkExtended = bookmarkExtended,
      fileBookmarkTime = bookmarkTime,
      fileBookmarkShared = bookmarkShared,
      fileBookmarkToRead = bookmarkToRead,
      fileBookmarkSelected = Just bookmarkSelected,
      fileBookmarkArchiveHref = bookmarkArchiveHref,
      fileBookmarkTags = tags
    }

readFileBookmarks :: (MonadIO m) => FilePath -> m (Either String [FileBookmark])
readFileBookmarks fpath =
  A.eitherDecode' . fromStrict <$> readFile fpath

insertFileBookmarks :: Key User -> FilePath -> DB (Either String Int)
insertFileBookmarks userId bookmarkFile = do
  mfmarks <- liftIO $ readFileBookmarks bookmarkFile
  case mfmarks of
    Left e -> pure $ Left e
    Right fmarks -> do
      bmarks <- liftIO $ mapM (fileBookmarkToBookmark userId) fmarks
      mbids <- mapM insertUnique bmarks
      mapM_ (void . insertUnique)
        $ concatMap (uncurry (mkBookmarkTags userId))
        $ catMaybes
        $ zipWith
          (\mbid tags -> (,tags) <$> mbid)
          mbids
          (extractTags <$> fmarks)
      pure $ Right (length bmarks)
  where
    extractTags = words . fileBookmarkTags

exportFileBookmarks :: Key User -> FilePath -> DB ()
exportFileBookmarks user fpath =
  liftIO . A.encodeFile fpath =<< getFileBookmarks
  where
    getFileBookmarks :: DB [FileBookmark]
    getFileBookmarks = do
      marks <- allUserBookmarks user
      pure $ fmap (\(bm, t) -> bookmarkTofileBookmark (entityVal bm) t) marks
