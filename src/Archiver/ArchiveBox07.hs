module Archiver.ArchiveBox07
  ( archiveBox07Backend,
  )
where

import Archiver.Backend
import ClassyPrelude
import Control.Monad.Logger (LoggingT, logDebug, logWarn, runLoggingT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.ByteString.Lazy qualified as BSL
import Data.Default (def)
import Data.Text qualified as T
import Database.Persist.Sql (Key)
import Model (Bookmark, Url (..), User, updateBookmarkArchiveUrl)
import Network.Connection qualified as NC
import Network.HTTP.Client
  ( CookieJar,
    HttpException (..),
    HttpExceptionContent (ResponseTimeout),
    Manager,
    Request (..),
    RequestBody (..),
    createCookieJar,
    httpLbs,
    insertCookiesIntoRequest,
    parseRequest_,
    redirectCount,
    responseBody,
    responseHeaders,
    responseStatus,
    responseTimeout,
    responseTimeoutMicro,
    updateCookieJar,
  )
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Network.HTTP.Types qualified as HT
import Network.HTTP.Types.Header (hLocation, hReferer)
import Network.HTTP.Types.Status qualified as NStatus
import Network.HTTP.Types.URI qualified as NUri
import Settings (AppSettings (..))
import Web.FormUrlEncoded qualified as WH
import Yesod.Default.Main (LogFunc)

data ArchiveBoxContext = ArchiveBoxContext
  { archiveBoxCookieJarRef :: IORef CookieJar,
    archiveBoxBaseUrl :: Text,
    archiveBoxPublicUrl :: Text,
    archiveBoxUsername :: Text,
    archiveBoxPassword :: Text,
    archiveBoxTag :: Text,
    archiveBoxPlugins :: Maybe Text,
    archiveBoxDB :: ArchiverDB,
    archiveBoxManager :: Manager
  }

data ArchiveBoxSubmitResult
  = ArchiveBoxSubmitAccepted
  | ArchiveBoxSubmitTimedOutWaiting
  | ArchiveBoxSubmitFailed Text

archiveBox07Backend ::
  AppSettings ->
  ArchiverDB ->
  LogFunc ->
  IO (Maybe ArchiverBackend)
archiveBox07Backend AppSettings {..} archiverDB logFunc = flip runLoggingT logFunc $ do
  case (nonBlank appArchiveBoxUrl, nonBlank appArchiveBoxUsername, nonBlank appArchiveBoxPassword) of
    (Nothing, _, _) -> $(logWarn) "Archive backend `archivebox` selected but archivebox-url missing; archiving disabled" >> pure Nothing
    (_, Nothing, _) -> $(logWarn) "Archive backend `archivebox` selected but archivebox-username missing; archiving disabled" >> pure Nothing
    (_, _, Nothing) -> $(logWarn) "Archive backend `archivebox` selected but archivebox-password missing; archiving disabled" >> pure Nothing
    (Just baseUrl, Just username, Just password) -> do
      cookieJarRef <- newIORef (createCookieJar [])
      archiveManager <- newTlsManagerWith $ mkManagerSettings def (NC.SockSettingsSimple <$> fmap unpack (appArchiveSocksProxyHost) <*> fmap toEnum (appArchiveSocksProxyPort))
      let publicUrl = fromMaybe baseUrl (nonBlank appArchiveBoxPublicUrl)
          ctx =
            ArchiveBoxContext
              { archiveBoxCookieJarRef = cookieJarRef,
                archiveBoxBaseUrl = baseUrl,
                archiveBoxPublicUrl = publicUrl,
                archiveBoxUsername = username,
                archiveBoxPassword = password,
                archiveBoxTag = appArchiveBoxTag,
                archiveBoxPlugins = nonBlank appArchiveBoxPlugins,
                archiveBoxDB = archiverDB,
                archiveBoxManager = archiveManager
              }
          denyHosts = catMaybes [extractHost baseUrl, extractHost publicUrl]
      pure $
        Just
          ArchiverBackend
            { runArchiver = \uid bid url ->
                flip runLoggingT logFunc $ _archiveBoxRun ctx uid bid url,
              isUrlDenylisted =
                \(Url inputUrl) ->
                  maybe False (\host -> any (hostMatches host) denyHosts) (extractHost inputUrl)
            }
  where
    hostMatches candidate denyHost = candidate == denyHost || ("." <> denyHost) `isSuffixOf` candidate
    extractHost rawUrl =
      let noScheme = fromMaybe rawUrl (stripPrefix "http://" rawUrl <|> stripPrefix "https://" rawUrl)
          authority = takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') noScheme
          host = toLower (takeWhile (/= ':') authority)
       in if null host then Nothing else Just host

_archiveBoxRun ::
  ArchiveBoxContext ->
  Key User ->
  Key Bookmark ->
  Url ->
  LoggingT IO ()
_archiveBoxRun ctx@ArchiveBoxContext {..} userId bookmarkId url = do
  loginResult <- _ensureArchiveBoxSession ctx
  case loginResult of
    Left warning -> $(logWarn) $ "ArchiveBox archiving skipped: " <> warning
    Right () -> do
      addPageResult <- liftIO $ _httpGetNoRedirect ctx "/add/"
      case addPageResult of
        Left err -> $(logWarn) $ "ArchiveBox add page request failed: " <> err
        Right (status, body, _)
          | status /= NStatus.status200 -> $(logWarn) $ "ArchiveBox add page returned unexpected status: " <> tshow status
          | not (_isAddPage body) -> $(logWarn) "ArchiveBox add page unavailable; session may have expired"
          | otherwise ->
              case extractAddCsrfToken body of
                Nothing -> $(logWarn) "ArchiveBox add page did not contain a CSRF token"
                Just csrf -> do
                  let archiveUrl = buildArchiveBoxLookupUrl archiveBoxPublicUrl url
                      formFields =
                        [ ("csrfmiddlewaretoken", csrf),
                          ("url", unUrl url),
                          ("parser", "url_list"),
                          ("tag", archiveBoxTag),
                          ("depth", "0")
                        ]
                          <> pluginFormFields archiveBoxPlugins
                  $(logDebug) $ "Archiving URL with ArchiveBox: " <> unUrl url
                  submitResult <- liftIO $ _httpPostAdd ctx formFields
                  case submitResult of
                    ArchiveBoxSubmitAccepted ->
                      storeArchiveUrl archiveUrl
                    ArchiveBoxSubmitTimedOutWaiting -> do
                      $(logDebug) $ "ArchiveBox accepted URL (timed out): " <> archiveUrl
                      storeArchiveUrl archiveUrl
                    ArchiveBoxSubmitFailed err -> $(logWarn) $ "ArchiveBox add request failed: " <> err
  where
    storeArchiveUrl archiveUrl = do
      $(logDebug) $ "storing archive link: " <> archiveUrl
      liftIO $ archiverRunDBWrite archiveBoxDB (updateBookmarkArchiveUrl userId bookmarkId (Just archiveUrl))
    pluginFormFields =
      maybe
        []
        ( map ("archive_methods",)
            . map T.strip
            . T.splitOn ","
            . T.filter (/= ' ')
        )
    extractAddCsrfToken body
      | _isAddPage body = _extractCsrfToken body
      | otherwise = Nothing
    buildArchiveBoxLookupUrl publicUrl (Url href) =
      _normalizeBaseUrl publicUrl <> "/archive/" <> encodeArchiveBoxPath href <> "/"
    encodeArchiveBoxPath href =
      let encoded = decodeUtf8 (NUri.urlEncode True (encodeUtf8 href))
       in T.replace "%2F" "/" $ T.replace "%3A" ":" encoded

_ensureArchiveBoxSession :: ArchiveBoxContext -> LoggingT IO (Either Text ())
_ensureArchiveBoxSession ctx = runExceptT $ do
  (status, body, _) <- ExceptT $ liftIO $ _httpGetNoRedirect ctx "/add/"
  if status == NStatus.status200 && _isAddPage body
    then pure ()
    else do
      ExceptT $ liftIO $ _archiveBoxLogin ctx
      (verifyStatus, verifyBody, _) <- ExceptT $ liftIO $ _httpGetNoRedirect ctx "/add/"
      if verifyStatus == NStatus.status200 && _isAddPage verifyBody
        then pure ()
        else throwE "ArchiveBox login failed or add page still unavailable"

_httpPostAdd :: ArchiveBoxContext -> [(Text, Text)] -> IO ArchiveBoxSubmitResult
_httpPostAdd ctx@ArchiveBoxContext {..} fields = do
  let body = _buildFormBody fields
      referer = _normalizeBaseUrl archiveBoxBaseUrl <> "/add/"
      request =
        (_buildArchiveBoxRequest ctx "/add/")
          { method = "POST",
            requestHeaders =
              [ (hReferer, encodeUtf8 referer),
                ("Accept", "text/html,application/xhtml+xml"),
                ("Content-Type", "application/x-www-form-urlencoded")
              ],
            requestBody = RequestBodyLBS body,
            redirectCount = 0,
            responseTimeout = responseTimeoutMicro archiveBoxAddResponseTimeoutMicro
          }
  _httpRequestRaw ctx "/add/" (const request) >>= \case
    Left ex
      | Just (HttpExceptionRequest _ ResponseTimeout) <- fromException ex ->
          pure ArchiveBoxSubmitTimedOutWaiting
      | otherwise ->
          pure $ ArchiveBoxSubmitFailed (tshow ex)
    Right (submitStatus, _, headers)
      | isArchiveBoxAddAccepted submitStatus headers ->
          pure ArchiveBoxSubmitAccepted
      | redirectsToLogin headers ->
          pure $ ArchiveBoxSubmitFailed "ArchiveBox add request rejected; not logged in"
      | otherwise ->
          pure $
            ArchiveBoxSubmitFailed $
              "ArchiveBox add request returned unexpected status: " <> tshow submitStatus
  where
    -- \| ArchiveBox runs extractors synchronously before responding to POST /add/, so
    -- we use a short timeout and treat ResponseTimeout as a successful enqueue.
    archiveBoxAddResponseTimeoutMicro = 10 * 1000000
    isArchiveBoxAddAccepted status headers =
      status == NStatus.status200
        || (status == NStatus.status302 && not (redirectsToLogin headers))
    redirectsToLogin =
      any
        ( \(headerName, headerValue) ->
            headerName == hLocation && "login" `T.isInfixOf` decodeUtf8 headerValue
        )

_archiveBoxLogin :: ArchiveBoxContext -> IO (Either Text ())
_archiveBoxLogin ctx@ArchiveBoxContext {..} = runExceptT $ do
  (status, body) <- ExceptT $ _httpGet ctx "/admin/login/"
  if status /= NStatus.status200
    then throwE $ "ArchiveBox login page returned unexpected status: " <> tshow status
    else case _extractCsrfToken body of
      Nothing -> throwE "ArchiveBox login page did not contain a CSRF token"
      Just csrf -> do
        (submitStatus, _) <-
          ExceptT $
            httpPostFormNoRedirect
              "/admin/login/"
              [ ("csrfmiddlewaretoken", csrf),
                ("username", archiveBoxUsername),
                ("password", archiveBoxPassword),
                ("next", "/add/")
              ]
        if submitStatus == NStatus.status200 || submitStatus == NStatus.status302
          then pure ()
          else throwE $ "ArchiveBox login returned unexpected status: " <> tshow submitStatus
  where
    httpPostFormNoRedirect ::
      Text -> [(Text, Text)] -> IO (Either Text (NStatus.Status, [HT.Header]))
    httpPostFormNoRedirect path fields = do
      let body = _buildFormBody fields
          referer = _normalizeBaseUrl archiveBoxBaseUrl <> path
          request =
            (_buildArchiveBoxRequest ctx path)
              { method = "POST",
                requestHeaders =
                  [ (hReferer, encodeUtf8 referer),
                    ("Accept", "text/html,application/xhtml+xml"),
                    ("Content-Type", "application/x-www-form-urlencoded")
                  ],
                requestBody = RequestBodyLBS body,
                redirectCount = 0
              }
      _httpRequest ctx path (const request) <&> \case
        Left err -> Left err
        Right (status, _, headers) -> Right (status, headers)

_httpGet :: ArchiveBoxContext -> Text -> IO (Either Text (NStatus.Status, Text))
_httpGet ctx path =
  _httpRequest ctx path id <&> \case
    Left err -> Left err
    Right (status, body, _) -> Right (status, body)

_httpGetNoRedirect :: ArchiveBoxContext -> Text -> IO (Either Text (NStatus.Status, Text, [HT.Header]))
_httpGetNoRedirect ctx path =
  _httpRequest ctx path (\req -> req {redirectCount = 0})

_httpRequest ::
  ArchiveBoxContext ->
  Text ->
  (Request -> Request) ->
  IO (Either Text (NStatus.Status, Text, [HT.Header]))
_httpRequest ctx path adjustRequest =
  _httpRequestRaw ctx path adjustRequest <&> \case
    Left ex -> Left (tshow ex)
    Right response -> Right response

_httpRequestRaw ::
  ArchiveBoxContext ->
  Text ->
  (Request -> Request) ->
  IO (Either SomeException (NStatus.Status, Text, [HT.Header]))
_httpRequestRaw ctx@ArchiveBoxContext {..} path adjustRequest = do
  now <- getCurrentTime
  cookieJar <- readIORef archiveBoxCookieJarRef
  let baseRequest = adjustRequest (_buildArchiveBoxRequest ctx path)
      (requestWithCookies, jarForSend) =
        insertCookiesIntoRequest baseRequest cookieJar now
  responseResult <- tryAny (httpLbs requestWithCookies archiveBoxManager)
  case responseResult of
    Left ex -> pure (Left ex)
    Right response -> do
      let (updatedJar, _) =
            updateCookieJar response requestWithCookies now jarForSend
      writeIORef archiveBoxCookieJarRef updatedJar
      pure $
        Right
          ( responseStatus response,
            decodeUtf8 (BSL.toStrict (responseBody response)),
            responseHeaders response
          )

_buildArchiveBoxRequest :: ArchiveBoxContext -> Text -> Request
_buildArchiveBoxRequest ArchiveBoxContext {..} path =
  let requestUrl = _normalizeBaseUrl archiveBoxBaseUrl <> path
   in parseRequest_ (unpack requestUrl)

_buildFormBody :: [(Text, Text)] -> BSL.ByteString
_buildFormBody fields =
  WH.urlEncodeAsForm
    ( map
        (\(key, value) -> (unpack key, unpack value))
        fields
    )

_extractCsrfToken :: Text -> Maybe Text
_extractCsrfToken html =
  let needle = "csrfmiddlewaretoken\" value=\""
   in case T.breakOn needle html of
        (_, rest)
          | not (T.null rest) ->
              Just $ T.takeWhile (/= '"') $ T.drop (T.length needle) rest
        _ -> Nothing

_isAddPage :: Text -> Bool
_isAddPage body = "id=\"add-form\"" `T.isInfixOf` body

_normalizeBaseUrl :: Text -> Text
_normalizeBaseUrl = T.dropWhileEnd (== '/')

nonBlank :: Maybe Text -> Maybe Text
nonBlank = (>>= nonEmptyText)
  where
    nonEmptyText value =
      let trimmed = T.strip value
       in if T.null trimmed then Nothing else Just trimmed
