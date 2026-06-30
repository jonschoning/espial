module Model.FileFirefox where

import ClassyPrelude.Yesod hiding (Value, exists, groupBy, on, (<=.), (==.), (>=.), (||.))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A (parseFail)
import Data.Time.Clock.POSIX qualified as TI (POSIXTime, posixSecondsToUTCTime)
import Model
import Model.Custom
import Types

-- * Firefox Bookmarks

data FirefoxBookmarkNode = FirefoxBookmarkNode
  { firefoxBookmarkChildren :: Maybe [FirefoxBookmarkNode],
    firefoxBookmarkDateAdded :: !TI.POSIXTime,
    firefoxBookmarkGuid :: !Text,
    firefoxBookmarkIconUri :: !(Maybe Text),
    firefoxBookmarkId :: !Int,
    firefoxBookmarkIndex :: !Int,
    firefoxBookmarkLastModified :: !TI.POSIXTime,
    firefoxBookmarkRoot :: !(Maybe Text),
    firefoxBookmarkTitle :: !Text,
    firefoxBookmarkType :: !Text,
    firefoxBookmarkTypeCode :: !Int,
    firefoxBookmarkUri :: !(Maybe Text)
  }
  deriving (Show, Eq, Typeable, Ord)

instance FromJSON FirefoxBookmarkNode where
  parseJSON (Object o) =
    FirefoxBookmarkNode
      <$> (o A..:? "children")
      <*> (o .: "dateAdded")
      <*> o
      .: "guid"
      <*> (o A..:? "iconUri")
      <*> o
      .: "id"
      <*> o
      .: "index"
      <*> (o .: "lastModified")
      <*> (o A..:? "root")
      <*> (o .: "title")
      <*> (o .: "type")
      <*> (o .: "typeCode")
      <*> (o A..:? "uri")
  parseJSON _ = A.parseFail "bad parse"

firefoxBookmarkNodeToBookmark :: UserId -> FirefoxBookmarkNode -> IO [Bookmark]
firefoxBookmarkNodeToBookmark user FirefoxBookmarkNode {..} =
  case firefoxBookmarkTypeCode of
    1 -> do
      slug <- mkBmSlug
      pure
        $ [ Bookmark
              { bookmarkUserId = user,
                bookmarkSlug = slug,
                bookmarkHref = fromMaybe "" firefoxBookmarkUri,
                bookmarkDescription = firefoxBookmarkTitle,
                bookmarkExtended = "",
                bookmarkTime = TI.posixSecondsToUTCTime (firefoxBookmarkDateAdded / 1000000),
                bookmarkShared = True,
                bookmarkToRead = False,
                bookmarkSelected = False,
                bookmarkArchiveHref = Nothing
              }
          ]
    2 ->
      join
        <$> mapM
          (firefoxBookmarkNodeToBookmark user)
          (fromMaybe [] firefoxBookmarkChildren)
    _ -> pure []

readFirefoxBookmarks :: (MonadIO m) => FilePath -> m (Either String FirefoxBookmarkNode)
readFirefoxBookmarks fpath =
  A.eitherDecode' . fromStrict <$> readFile fpath

insertFirefoxBookmarks :: Key User -> FirefoxBookmarkNode -> DB Int
insertFirefoxBookmarks userId fmarks = do
  bmarks <- liftIO $ firefoxBookmarkNodeToBookmark userId fmarks
  mapM_ (void . insertUnique) bmarks
  pure (length bmarks)
