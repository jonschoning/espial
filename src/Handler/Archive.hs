module Handler.Archive where

import Import
import Data.Function ((&))
import Data.Char (ord)
import qualified Data.Attoparsec.ByteString.Char8 as AP8
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Types.Status as NH
import qualified Web.FormUrlEncoded as WH
import HTMLEntities.Decoder (htmlEncodedText)
import Data.Text.Lazy.Builder (toLazyText)
import Network.Wai (requestHeaderHost)
import qualified Network.Connection as NC

shouldArchiveBookmark :: User -> Key Bookmark -> Handler Bool
shouldArchiveBookmark user kbid =
  runDB (get kbid) >>= \case
  Nothing -> pure False

  Just bm -> do
    pure $
      isNothing (bookmarkArchiveHref bm) &&
      bookmarkShared bm
      && not (_isArchiveBlacklisted bm)
      && userArchiveDefault user

getArchiveManager :: Handler Manager
getArchiveManager = do
  appSettings <- appSettings <$> getYesod
  let mSocks =
        NC.SockSettingsSimple <$>
        fmap unpack (appArchiveSocksProxyHost appSettings) <*>
        fmap toEnum (appArchiveSocksProxyPort appSettings)
  NH.newTlsManagerWith (NH.mkManagerSettings def mSocks)


archiveBookmarkUrl :: Key Bookmark -> String -> Handler ()
archiveBookmarkUrl kbid url =
  (_fetchArchiveSubmitInfo >>= \case
    Left e -> do
      $(logError) (pack e)
    Right submitInfo ->  do
        userId <- requireAuthId
        req <- _buildArchiveSubmitRequest submitInfo url
        manager <- getArchiveManager
        res <- liftIO $ NH.httpLbs req manager
        let status = NH.responseStatus res
        let updateArchiveUrl url' = runDB $ updateBookmarkArchiveUrl userId kbid $ Just url'
            headers = NH.responseHeaders res
        case status of
          s | s == NH.status200 ->
            for_ (lookup "Refresh" headers >>= _parseRefreshHeaderUrl) updateArchiveUrl
          s | s == NH.status302 || s == NH.status307 ->
            for_ (lookup "Location" headers) (updateArchiveUrl . decodeUtf8)
          _ -> $(logError) (pack (show res)))
  `catch` (\(e::SomeException) -> ($(logError) $ (pack.show) e) >> throwIO e)

_isArchiveBlacklisted :: Bookmark -> Bool
_isArchiveBlacklisted Bookmark {..} =
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
  if not (null u)
    then Just $ decodeUtf8 u
    else Nothing

_fetchArchiveSubmitInfo :: Handler (Either String (String , String))
_fetchArchiveSubmitInfo = do
  req <- buildRequest "https://archive.li/"
  manager <- getArchiveManager
  res <- liftIO $ NH.httpLbs req manager
  let body = LBS.toStrict (responseBody res)
      action = _parseSubstring (AP8.string "action=\"") (AP8.notChar '"') body
      submitId = _parseSubstring (AP8.string "submitid\" value=\"") (AP8.notChar '"') body
  if statusCode (responseStatus res) == 200
    then pure $ (,) <$> action <*> submitId
    else pure $ Left $ "Invalid statusCode: " <> show (responseStatus res)


_parseSubstring :: AP8.Parser ByteString -> AP8.Parser Char -> BS.ByteString -> Either String String
_parseSubstring start inner = AP8.parseOnly (skipAnyTill start >> AP8.many1 inner)
  where
    skipAnyTill end = go where go = end $> () <|> AP8.anyChar *> go


fetchPageTitle :: String -> Handler (Either String Text)
fetchPageTitle url =
  do
     req <- buildRequest url
     res <- liftIO $ NH.httpLbs req =<< NH.getGlobalManager
     let body = LBS.toStrict (responseBody res)
     pure (decodeHtmlBs <$> parseTitle body)
     `catch` (\(e :: SomeException) -> do
                $(logError) $ (pack . show) e
                pure (Left (show e)))
  where
    parseTitle bs =
      flip AP.parseOnly bs do
        _ <- skipAnyTill (AP.string "<title")
        _ <- skipAnyTill (AP.string ">")
        let lt = toEnum (ord '<')
        AP.takeTill (== lt)
    decodeHtmlBs = toStrict . toLazyText . htmlEncodedText . decodeUtf8
    skipAnyTill end = go where go = end $> () <|> AP.anyWord8 *> go

_buildArchiveSubmitRequest :: (String, String) -> String -> Handler NH.Request
_buildArchiveSubmitRequest (action, submitId) href = do
  req <- buildRequest ("POST " <> action)
  pure $ req
    { NH.requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : NH.requestHeaders req
    , NH.requestBody =
        NH.RequestBodyLBS $
        WH.urlEncodeAsForm
            ([("submitid", submitId), ("url", href)] :: [(String, String)])
    , NH.redirectCount = 0
    }

buildRequest :: String -> Handler Request
buildRequest url = do
  ua <- _archiveUserAgent
  pure $ NH.parseRequest_ url & \r ->
    r { NH.requestHeaders =
          [ ("Cache-Control", "max-age=0")
          , ("User-Agent", ua)
          ]
      }

_archiveUserAgent :: Handler ByteString
_archiveUserAgent = do
  mHost <- requestHeaderHost . reqWaiRequest <$> getRequest
  pure $ "espial-" <> maybe "" (BS8.takeWhile (/= ':')) mHost

