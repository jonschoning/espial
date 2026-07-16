-- | Common handler functions.
module Handler.Common where

import Data.Aeson qualified as A
import Data.FileEmbed (embedFile)
import Data.Text qualified as T
import Import
import Text.Read
import Util (format8601z, parseTimeText)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 5
  -- cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  pure
    $ TypedContent "image/x-icon"
    $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
  pure
    $ TypedContent typePlain
    $ toContent $(embedFile "config/robots.txt")

lookupPagingParams :: Maybe Text -> Handler (Maybe Int64, Maybe Int64)
lookupPagingParams prefix =
  (,)
    <$> getsetUrlSessionParam "count" (fromMaybe "" prefix)
    <*> getUrlParam (fromMaybe "" prefix <> "page")

pagingCursorBeforeParam :: Text
pagingCursorBeforeParam = "before"

pagingCursorAfterParam :: Text
pagingCursorAfterParam = "after"

sortParam :: Text
sortParam = "sort"

orderParam :: Text
orderParam = "order"

-- | "~" is unreserved in URL queries and cannot occur in the ISO8601 time
bookmarkCursorSeparator :: Text
bookmarkCursorSeparator = "~"

formatEntityPagingCursorBm :: Entity Bookmark -> Text
formatEntityPagingCursorBm (Entity bid bm) =
  format8601z (bookmarkTime bm) <> bookmarkCursorSeparator <> tshow (fromSqlKey bid)

parsePagingCursorBm :: Text -> Maybe BookmarkCursor
parsePagingCursorBm t = case T.splitOn bookmarkCursorSeparator t of
  [timeText] -> flip BookmarkCursor Nothing <$> parseTimeText timeText
  [timeText, idText] ->
    BookmarkCursor
      <$> parseTimeText timeText
      <*> (Just <$> parsePagingCursorKey idText)
  _ -> Nothing

formatEntityPagingCursorTimeNt :: Entity Note -> Text
formatEntityPagingCursorTimeNt = format8601z . noteCreated . entityVal

parsePagingCursorTime :: Text -> Maybe UTCTime
parsePagingCursorTime = parseTimeText

formatEntityPagingCursorKey :: (ToBackendKey SqlBackend record) => Entity record -> Text
formatEntityPagingCursorKey = tshow . fromSqlKey . entityKey

parsePagingCursorKey :: (ToBackendKey SqlBackend record) => Text -> Maybe (Key record)
parsePagingCursorKey t =
  toSqlKey <$> (readMaybe (unpack t) :: Maybe Int64)

parsePagingCursorParams ::
  (Text -> Maybe cursor) ->
  (Text -> Maybe cursor) ->
  Maybe Text ->
  Maybe Text ->
  Maybe cursor
parsePagingCursorParams mkBefore mkAfter mbefore mafter =
  (mkBefore =<< mbefore) <|> (mkAfter =<< mafter)

espialUserAgent :: UserAgent
espialUserAgent = UserAgent "espial"

browserUserAgent :: UserAgent
browserUserAgent =
  UserAgent
    $ pack
    $ "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
    <> "AppleWebKit/537.36 (KHTML, like Gecko) "
    <> "Chrome/136.0.0.0 Safari/537.36"

getUrlParam :: (Read a) => Text -> Handler (Maybe a)
getUrlParam name = do
  fmap parseMaybe (lookupGetParam name)
  where
    parseMaybe x = readMaybe . unpack =<< x

getsetUrlSessionParam ::
  forall a.
  (Show a, Read a) =>
  Text ->
  Text ->
  Handler (Maybe a)
getsetUrlSessionParam name sessionPrefix = do
  p <- fmap parseMaybe (lookupGetParam name)
  s <- fmap parseMaybe (lookupSession (sessionPrefix <> name))
  for_ p (setSession (sessionPrefix <> name) . (pack . show))
  pure (p <|> s)
  where
    parseMaybe :: Maybe Text -> Maybe a
    parseMaybe x = readMaybe . unpack =<< x

lookupTagCloudMode :: (MonadHandler m) => m (Maybe TagCloudMode)
lookupTagCloudMode = do
  (A.decode . fromStrict =<<) <$> lookupSessionBS "tagCloudMode"

setTagCloudMode :: (MonadHandler m) => TagCloudMode -> m ()
setTagCloudMode = setSessionBS "tagCloudMode" . toStrict . A.encode

getTagCloudMode :: (MonadHandler m) => Bool -> Bool -> [Tag] -> m TagCloudMode
getTagCloudMode isowner publicTagCloud tags = do
  ms <- lookupTagCloudMode
  let expanded = maybe False isExpanded ms
  if not isowner && not publicTagCloud
    then pure TagCloudModeNone
    else
      if not (null tags)
        then pure $ TagCloudModeRelated False tags
        else pure $ case ms of
          Nothing -> TagCloudModeTop expanded
          Just (TagCloudModeRelated e _) -> TagCloudModeTop e
          Just (TagCloudModeRelatedLowerBound e _ _) -> TagCloudModeTop e
          Just m -> m
