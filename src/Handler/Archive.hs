module Handler.Archive where

import Import
import Data.Function ((&))
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Types.Status as NH
import qualified Web.FormUrlEncoded as WH
import qualified Control.Monad.Metrics as MM

shouldArchiveBookmark :: User -> Key Bookmark -> Handler Bool
shouldArchiveBookmark user kbid = do
  runDB (get kbid) >>= \case
    Nothing -> pure False
    Just bm -> do
      pure $
        (isNothing $ bookmarkArchiveHref bm) &&
        (bookmarkShared bm)
        && not (_isArchiveBlacklisted bm)
        && not (userPrivacyLock user)
        && userArchiveDefault user

archiveBookmarkUrl :: Key Bookmark -> String -> Handler ()
archiveBookmarkUrl kbid url =
  (_fetchArchiveSubmitInfo >>= \case
    Left e -> do
      MM.increment "archive.fetchSubmitId_noparse"
      $(logError) (pack e)
    Right submitInfo ->  do
        userId <- requireAuthId
        let req = _buildArchiveSubmitRequest submitInfo url
        MM.increment "archive.submit"  
        res <- liftIO $ NH.httpLbs req  =<< NH.getGlobalManager
        let status = NH.responseStatus res
        MM.increment ("archive.submit_status_" <> (pack.show) (NH.statusCode status)) 
        let updateArchiveUrl = runDB . updateBookmarkArchiveUrl userId kbid . Just
            headers = NH.responseHeaders res
        case status of
          s | s == NH.status200 ->
            for_ (lookup "Refresh" headers >>= _parseRefreshHeaderUrl) updateArchiveUrl
          s | s == NH.status302 -> 
            for_ (lookup "Location" headers) (updateArchiveUrl . decodeUtf8)
          _ -> $(logError) (pack (show res)))
  `catch` (\(e::SomeException) -> ($(logError) $ (pack.show) e) >> throwIO e)

_isArchiveBlacklisted :: Bookmark -> Bool
_isArchiveBlacklisted (Bookmark {..}) =
  [ "hulu"
  , "livestream"
  , "netflix"
  , "skillsmatter"
  , "twitch.tv"
  , "vimeo"
  , "youtu.be"
  , "youtube"
  , "archive."
  ] &
  any (`isInfixOf` bookmarkHref)

_parseRefreshHeaderUrl :: ByteString -> Maybe Text
_parseRefreshHeaderUrl h = do
  let u = BS8.drop 1 $ BS8.dropWhile (/= '=') h
  if (not (null u))
    then Just $ decodeUtf8 u
    else Nothing

_buildArchiveSubmitRequest :: (String, String) -> String -> NH.Request
_buildArchiveSubmitRequest (action, submitId) href =
  NH.parseRequest_ ("POST " <> action) & \r ->
    r { NH.requestHeaders =
        [ ("User-Agent", _archiveUserAgent)
        , ("Content-Type", "application/x-www-form-urlencoded")
        ]
      , NH.requestBody = NH.RequestBodyLBS $ WH.urlEncodeAsForm ((
        [ ("submitid" , submitId)
        , ("url", href)
        ]) :: [(String, String)])
      , NH.redirectCount = 0
      }

_fetchArchiveSubmitInfo :: Handler (Either String (String , String))
_fetchArchiveSubmitInfo = do
  MM.increment "archive.fetchSubmitId"  
  res <- liftIO $ NH.httpLbs buildSubmitRequest =<< NH.getGlobalManager
  MM.increment ("archive.fetchSubmitId_status_" <> (pack.show) (NH.statusCode (NH.responseStatus res))) 
  let body = LBS.toStrict (responseBody res)
      action = _parseSubstring (AP.string "action=\"") (AP.notChar '"') body
      submitId = _parseSubstring (AP.string "submitid\" value=\"") (AP.notChar '"') body
  pure $ (,) <$> action <*> submitId
  where
    buildSubmitRequest =
      NH.parseRequest_ "https://archive.li/" & \r ->
        r {NH.requestHeaders = [("User-Agent", _archiveUserAgent)]}

_archiveUserAgent :: ByteString
_archiveUserAgent = "espial"

_parseSubstring :: AP.Parser ByteString -> AP.Parser Char -> BS.ByteString -> Either String String
_parseSubstring start inner res = do
  (flip AP.parseOnly) res (skipAnyTill start >> AP.many1 inner)
  where
    skipAnyTill end = go where go = end *> pure () <|> AP.anyChar *> go
