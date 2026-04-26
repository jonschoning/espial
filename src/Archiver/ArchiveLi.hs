module Archiver.ArchiveLi
  ( archiveLiBackend,
  )
where

import Archiver.Backend
import ClassyPrelude
import Control.Monad.Logger (LoggingT, logDebug, runLoggingT)
import qualified Data.Attoparsec.ByteString.Char8 as AP8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Database.Persist.Sql (ConnectionPool, Key, runSqlPool)
import Model (Bookmark, Url (..), User, UserAgent (UserAgent), updateBookmarkArchiveUrl)
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types.Status as NH
import qualified Web.FormUrlEncoded as WH
import Yesod.Default.Main (LogFunc)

-- | Archive.li backend. The supplied 'Manager' is reused for all requests.
archiveLiBackend :: ConnectionPool -> NH.Manager -> LogFunc -> ArchiverBackend
archiveLiBackend connPool manager logFunc =
  ArchiverBackend
    { runArchiver = \uid bid ua url -> flip runLoggingT logFunc $ _archiveLiRun connPool manager uid bid ua url,
      isUrlDenylisted = \(Url url) -> any (`isInfixOf` url) _archiveLiDenylist
    }

_archiveLiDenylist :: [Text]
_archiveLiDenylist =
  [ "hulu",
    "livestream",
    "netflix",
    "skillsmatter",
    "twitch.tv",
    "vimeo",
    "youtu.be",
    "youtube",
    "archive."
  ]

_archiveLiRun :: ConnectionPool -> NH.Manager -> Key User -> Key Bookmark -> UserAgent -> Url -> LoggingT IO ()
_archiveLiRun connPool manager userId bookmarkId ua url = do
  $(logDebug) $ "Archiving URL: " <> (unUrl url)
  _fetchArchiveLiSubmitInfo manager ua >>= \case
    Left err -> $(logDebug) $ "Failed to fetch submit info: " <> pack err
    Right (submitInfo, cookieJar) -> do
      let req = _buildArchiveLiSubmitRequest ua submitInfo cookieJar url
      $(logDebug) $ "Archive.li request: " <> tshow req
      res <- liftIO $ NH.httpLbs req manager
      let status = NH.responseStatus res
          headers = NH.responseHeaders res
          mArchiveUrl =
            if
              | status == NH.status200 ->
                  (lookup "Refresh" headers >>= _parseRefreshHeaderUrl)
              | status == NH.status302 || status == NH.status307 ->
                  (lookup "Location" headers >>= \h -> Just (decodeUtf8 h))
              | otherwise -> Nothing
      $(logDebug) $ "Archive response status: " <> tshow status <> ", URL result: " <> tshow mArchiveUrl
      forM_ mArchiveUrl $ \archiveUrl ->
        liftIO $ runSqlPool (updateBookmarkArchiveUrl userId bookmarkId (Just archiveUrl)) connPool

_fetchArchiveLiSubmitInfo :: NH.Manager -> UserAgent -> LoggingT IO (Either String (Url, NH.CookieJar))
_fetchArchiveLiSubmitInfo manager ua = do
  $(logDebug) "Fetching archive.li submit info"
  let req = _mkRequest ua (Url "https://archive.li/")
  res <- liftIO $ NH.httpLbs req manager
  let body = LBS.toStrict (NH.responseBody res)
      action = Url . pack <$> _parseSubstring (AP8.string "action=\"") (AP8.notChar '"') body
      cookieJar = NH.responseCookieJar res
  if NH.statusCode (NH.responseStatus res) == 200
    then do
      $(logDebug) $ "Successfully fetched archive.li form: action=" <> tshow action
      pure $ (,cookieJar) <$> action
    else do
      let errMsg = "Invalid statusCode: " <> show (NH.responseStatus res)
      $(logDebug) $ pack errMsg
      pure $ Left errMsg

_buildArchiveLiSubmitRequest :: UserAgent -> Url -> NH.CookieJar -> Url -> NH.Request
_buildArchiveLiSubmitRequest ua (Url action) cookieJar (Url href) =
  let query =
        unpack
          . decodeUtf8
          . LBS.toStrict
          $ WH.urlEncodeAsForm ([("url", unpack href)] :: [(String, String)])
      req = _mkRequest ua (Url (action <> "?" <> pack query))
   in req
        { NH.cookieJar = Just cookieJar,
          NH.redirectCount = 0
        }

_mkRequest :: UserAgent -> Url -> NH.Request
_mkRequest (UserAgent ua) (Url url) =
  NH.parseRequest_ (unpack url) & \r ->
    r
      { NH.requestHeaders =
          [ ("Cache-Control", "max-age=0"),
            ("User-Agent", encodeUtf8 ua)
          ]
      }

_parseRefreshHeaderUrl :: ByteString -> Maybe Text
_parseRefreshHeaderUrl h =
  let u = BS8.drop 1 $ BS8.dropWhile (/= '=') h
   in if not (null u) then Just (decodeUtf8 u) else Nothing

_parseSubstring :: AP8.Parser ByteString -> AP8.Parser Char -> BS.ByteString -> Either String String
_parseSubstring start inner = AP8.parseOnly (skipAnyTill start >> AP8.many1 inner)
  where
    skipAnyTill end = go where go = end $> () <|> AP8.anyChar *> go
