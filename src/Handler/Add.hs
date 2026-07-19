module Handler.Add where

import Data.Aeson qualified as A
import Data.Attoparsec.ByteString qualified as AP
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Either (isRight)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Handler.Archive
import Handler.Common (browserUserAgent)
import Import
import Network.HTTP.Client qualified as NH
import Network.HTTP.Client.TLS qualified as NHT
import Network.HTTP.Types.URI qualified as NUri
import Network.PrivateAddress (isDisallowedFetchHost)
import Util

-- * View

getAddViewR :: Handler Html
getAddViewR = do
  Entity userId user <- requireAuth
  yesod <- getYesod
  let frontendBundleName = appFrontendBundleName yesod
      archiveBackendEnabled = isJust (appArchiver yesod)
  murl <- lookupGetParam "url"
  mBookmarkDb <- runDB (fetchBookmarkByUrl userId murl)
  newBookmarkForm <- mkNewBookmarkFormForUrl archiveBackendEnabled
  let mExistingBookmarkForm = toBookmarkForm <$> mBookmarkDb
      bookmarkForm = mExistingBookmarkForm <|> Just newBookmarkForm
      renderEl = "addForm" :: Text

  popupLayout do
    toWidget
      [whamlet|
      <div id="#{ renderEl }">
    |]
    toWidgetBody
      [julius|
      app.dat.bmark = #{ toJSON bookmarkForm };
      app.dat.archiveBackendEnabled = #{ archiveBackendEnabled };
      app.dat.suggestTags = #{ userSuggestTags user };
      app.dat.suggestTagsUseReturnKey = #{ userSuggestTagsUseReturnKey user };
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderAddForm } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        renderAddForm('##{renderEl}')(app.dat.bmark)();
    |]
  where
    mkNewBookmarkFormForUrl :: Bool -> Handler BookmarkForm
    mkNewBookmarkFormForUrl archiveBackendEnabled = do
      Entity _ user <- requireAuth
      url <- lookupGetParam "url" <&> fromMaybe ""
      title <- lookupGetParam "title"
      description <- lookupGetParam "description" <&> fmap Textarea
      tags <- lookupGetParam "tags"
      private <- lookupGetParam "private" <&> fmap parseChk <&> (<|> Just (userPrivateDefault user))
      toread <- lookupGetParam "toread" <&> fmap parseChk
      pure (mkNewBookmarkForm archiveBackendEnabled user url title description tags private toread)
      where
        parseChk s = s == "yes" || s == "on" || s == "true" || s == "1"

-- * API

postAddR :: Handler Text
postAddR = do
  app <- getYesod
  (_, authUser) <- requireAuthPair
  lang <- getCurrentLang (LangSourceUser (Just authUser))
  let t key = appTranslate app lang (I18nKey key)
  bookmarkForm <- requireCheckJsonBody
  handleFormSuccess bookmarkForm >>= \case
    Created bid -> sendStatusJSON created201 bid
    Updated _ -> sendResponseStatus noContent204 ()
    Failed reason -> sendResponseStatus status400 (translateFailedReason t reason)
  where
    handleFormSuccess :: BookmarkForm -> Handler (UpsertResult (Key Bookmark))
    handleFormSuccess bookmarkForm = do
      (userId, user) <- requireAuthPair
      appSettings <- appSettings <$> getYesod
      case (appAllowNonHttpUrlSchemes appSettings, (parseRequest . unpack . _url) bookmarkForm) of
        (False, Nothing) -> pure $ Failed (ReasonInvalidInput "")
        _ -> do
          let mkbid = toSqlKey <$> _bid bookmarkForm
              tags = maybe [] normalizeTags (_tags bookmarkForm)
          bm <- liftIO $ bookmarkFormToBookmark userId bookmarkForm
          res <- runDBWrite (upsertBookmark userId mkbid bm tags)
          case res of
            Created kbid | fromMaybe (userArchiveDefault user) (_archiveRequested bookmarkForm) -> _archive kbid bm
            Updated kbid | isJust (_archiveRequested bookmarkForm) -> _archive kbid bm
            _ -> pure ()
          pure res
    _archive :: Key Bookmark -> Bookmark -> Handler ()
    _archive = archiveBookmarkUrl
    translateFailedReason :: (Text -> Text) -> FailedReason -> Text
    translateFailedReason t reason = case reason of
      ReasonUnauthorized -> t "error.upsertUnauthorized"
      ReasonNotFound -> t "error.upsertNotFound"
      ReasonHrefUsedByOther -> t "error.upsertHrefUsedByOther"
      ReasonConflictWithNewer -> t "error.upsertConflictWithNewer"
      ReasonInvalidInput detail -> if null detail then t "error.upsertInvalidInput" else detail

postAddBulkR :: Handler Text
postAddBulkR = do
  bookmarkForms <- requireCheckJsonBody
  maxItems <- appAddBulkMaxItems . appSettings <$> getYesod
  when (length bookmarkForms > maxItems) do
    sendResponseStatus status413 ("Too many items: max " <> tshow maxItems :: Text)
  ress <- handleFormSuccess bookmarkForms
  sendStatusJSON ok200 (fmap _toJson ress)
  where
    handleFormSuccess :: [BookmarkForm] -> Handler [UpsertResult (Key Bookmark)]
    handleFormSuccess bookmarkForms = do
      (userId, user) <- requireAuthPair
      appSettings <- appSettings <$> getYesod
      let allowNonHttpUrlSchemes = appAllowNonHttpUrlSchemes appSettings
          urlResults = parseRequest . unpack . _url <$> bookmarkForms
          marked =
            [ if allowNonHttpUrlSchemes || isRight urlResult
                then Right bookmarkForm
                else Left $ ReasonInvalidInput $ either (("Invalid URL: " <>) . pack . show) (const "Invalid URL") urlResult
            | (bookmarkForm, urlResult) <- zip bookmarkForms urlResults
            ]
          validForms = [bookmarkForm | Right bookmarkForm <- marked]
          mkbids :: [Maybe (Key Bookmark)] = (fmap toSqlKey . _bid) <$> validForms
          tagss = maybe [] normalizeTags <$> (fmap _tags validForms)
      bms <- liftIO $ traverse (bookmarkFormToBookmark userId) validForms
      validRess <- runDBWrite (upsertBookmarks userId mkbids bms tagss)
      let toArchive =
            catMaybes
              [ case res of
                  Created kbid | fromMaybe (userArchiveDefault user) (_archiveRequested bookmarkForm) -> Just (kbid, bm)
                  Updated kbid | isJust (_archiveRequested bookmarkForm) -> Just (kbid, bm)
                  _ -> Nothing
              | (res, bm, bookmarkForm) <- zip3 validRess bms validForms
              ]
      unless (null toArchive) (archiveBookmarkUrls toArchive)
      pure (mergeBulkAddResults marked validRess)

    -- \| Re-interleaves per-item validation failures with the upsert results of the valid subset, restoring the original request order.
    mergeBulkAddResults :: [Either FailedReason BookmarkForm] -> [UpsertResult (Key Bookmark)] -> [UpsertResult (Key Bookmark)]
    mergeBulkAddResults (Left err : rest) valids = Failed (err) : mergeBulkAddResults rest valids
    mergeBulkAddResults (Right _ : rest) (v : valids) = v : mergeBulkAddResults rest valids
    mergeBulkAddResults _ _ = []

    _toJson (Created a) = object ["status" .= ("created" :: Text), "id" .= a]
    _toJson (Updated a) = object ["status" .= ("updated" :: Text), "id" .= a]
    _toJson (Failed reason) = object ["status" .= ("failed" :: Text), "reason" .= failedReasonTag reason, "detail" .= failedReasonDetail reason]
      where
        failedReasonTag :: FailedReason -> Text
        failedReasonTag = \case
          ReasonUnauthorized -> "unauthorized"
          ReasonNotFound -> "not_found"
          ReasonConflictWithNewer -> "conflict_with_newer"
          ReasonHrefUsedByOther -> "href_used_by_other"
          ReasonInvalidInput _ -> "invalid_input"
        failedReasonDetail (ReasonInvalidInput detail) | not (null detail) = Just detail
        failedReasonDetail _ = Nothing

postTagSuggestionsR :: Handler Text
postTagSuggestionsR = do
  userId <- requireAuthId
  tagSuggestionRequest <- requireCheckJsonBody
  let suggestion_limit = 10
  tagSuggestions <- runDB $ getTagSuggestions userId tagSuggestionRequest suggestion_limit
  sendStatusJSON ok200 tagSuggestions

postLookupTitleR :: Handler ()
postLookupTitleR = do
  void requireAuthId
  bookmarkForm <- (requireCheckJsonBody :: Handler BookmarkForm)
  fetchPageTitle (Url (_url bookmarkForm)) >>= \case
    Left _ -> sendResponseStatus noContent204 ()
    Right title -> sendResponseStatus ok200 title

data RequestOverride = RequestOverride
  { requestOverrideMatch :: NH.Request -> Bool,
    requestOverrideTransform :: Url -> Url,
    requestOverrideParseTitle :: ByteString -> Either String Text
  }

newtype OEmbedResponse = OEmbedResponse Text

instance A.FromJSON OEmbedResponse where
  parseJSON = A.withObject "OEmbedResponse" \o -> OEmbedResponse <$> o A..: "title"

fetchPageTitle :: Url -> Handler (Either String Text)
fetchPageTitle url = do
  $(logDebug) $ "fetchPageTitle: looking up title for " <> unUrl url
  result <-
    (buildRequest url >>= \(req, parseBody) -> fetchFollowingRedirects maxRedirects req parseBody)
      `catch` ( \(e :: SomeException) -> do
                  $(logError) $ "fetchPageTitle: " <> unUrl url <> " raised " <> (pack . show) e
                  pure (Left (show e))
              )
  $(logDebug) $ "fetchPageTitle: result for " <> unUrl url <> " = " <> tshow result
  pure result
  where
    maxTitleFetchBytes :: Int
    maxTitleFetchBytes = 1024 * 1024

    maxRedirects :: Int
    maxRedirects = 5
    -- redirectCount is 0 on every request built here so each hop can be
    -- re-matched against requestOverrides before it's sent. parseBody is
    -- carried alongside the request since an override may swap in a
    -- differently-shaped response (e.g. reddit's JSON oEmbed API).
    fetchFollowingRedirects :: Int -> NH.Request -> (ByteString -> Either String Text) -> Handler (Either String Text)
    fetchFollowingRedirects hopsLeft req parseBody = do
      let reqHost = decodeUtf8 (NH.host req)
      $(logDebug) $ "fetchPageTitle: requesting " <> reqHost <> decodeUtf8 (NH.path req) <> " (hopsLeft=" <> tshow hopsLeft <> ")"
      disallowed <- liftIO $ isDisallowedFetchHost (unpack reqHost)
      if disallowed
        then do
          $(logDebug) $ "fetchPageTitle: host disallowed: " <> reqHost
          pure (Left "URL host is not allowed")
        else do
          manager <- liftIO NHT.getGlobalManager
          (status, step) <- liftIO $ NH.withResponse req manager \resp -> do
            let status = NH.responseStatus resp
            (,) status
              <$> case redirectLocation resp of
                Just location | hopsLeft > 0 -> pure (Left location)
                _ -> Right <$> readBoundedBody (NH.responseBody resp)
          $(logDebug) $ "fetchPageTitle: " <> reqHost <> " responded " <> tshow status
          case step of
            Left location -> do
              $(logDebug) $ "fetchPageTitle: received redirect from " <> reqHost <> " to " <> decodeUtf8 location
              (nextReq, nextParseBody) <- buildRequest (Url (decodeUtf8 location))
              fetchFollowingRedirects (hopsLeft - 1) nextReq nextParseBody
            Right body
              | statusIsRedirection status -> do
                  $(logWarn) $ "fetchPageTitle: max redirects reached at " <> reqHost
                  pure (parseBody body)
              | otherwise -> pure (parseBody body)
    redirectLocation resp
      | statusCode (NH.responseStatus resp) `elem` [301, 302, 303, 307, 308] =
          lookup "Location" (NH.responseHeaders resp)
      | otherwise = Nothing
    buildRequest :: Url -> Handler (NH.Request, ByteString -> Either String Text)
    buildRequest u = do
      let UserAgent ua = browserUserAgent
          mkReq u' =
            (NH.parseRequest_ (unpack (unUrl u')))
              { NH.redirectCount = 0,
                NH.requestHeaders =
                  [ ("Cache-Control", "max-age=0"),
                    ("User-Agent", encodeUtf8 ua)
                  ]
              }
          req = mkReq u
      case find (\o -> requestOverrideMatch o req) requestOverrides of
        Just o -> do
          let u' = requestOverrideTransform o u
          $(logDebug) $ "fetchPageTitle: applying request override, " <> unUrl u <> " -> " <> unUrl u'
          pure (mkReq u', requestOverrideParseTitle o)
        Nothing -> pure (req, parseTitle)
    readBoundedBody br = BS.concat <$> go 0
      where
        go total
          | total >= maxTitleFetchBytes = pure []
          | otherwise = do
              chunk <- NH.brRead br
              if null chunk then pure [] else (chunk :) <$> go (total + BS.length chunk)
    parseTitle bs =
      flip AP.parseOnly bs do
        _ <- skipAnyTill (AP.string "<title")
        _ <- skipAnyTill (AP.string ">")
        let lt = toEnum (ord '<')
        decodeHtmlBs <$> AP.takeTill (== lt)
      where
        skipAnyTill end = go where go = end $> () <|> AP.anyWord8 *> go
        decodeHtmlBs = toStrict . toLazyText . htmlEncodedText . decodeUtf8
    requestOverrides =
      [ RequestOverride
          { requestOverrideMatch = \r ->
              NH.host r
                `elem` (["reddit.com", "www.reddit.com", "np.reddit.com", "old.reddit.com"] :: [ByteString])
                && not ("/s/" `BS.isInfixOf` NH.path r),
            requestOverrideTransform = oEmbedTransform "https://www.reddit.com/oembed",
            requestOverrideParseTitle = parseOEmbedTitle
          },
        -- youtu.be share links are accepted by the oEmbed endpoint directly, no redirect to resolve first.
        RequestOverride
          { requestOverrideMatch = \r ->
              NH.host r `elem` (["youtube.com", "www.youtube.com", "m.youtube.com", "youtu.be"] :: [ByteString]),
            requestOverrideTransform = oEmbedTransform "https://www.youtube.com/oembed",
            requestOverrideParseTitle = parseOEmbedTitle
          },
        -- vm.tiktok.com/vt.tiktok.com share links 400 against the oEmbed endpoint, so leave those
        -- unmatched until they redirect to a canonical www.tiktok.com/@user/video/<id> URL.
        RequestOverride
          { requestOverrideMatch = \r ->
              NH.host r `elem` (["tiktok.com", "www.tiktok.com"] :: [ByteString]),
            requestOverrideTransform = oEmbedTransform "https://www.tiktok.com/oembed",
            requestOverrideParseTitle = parseOEmbedTitle
          },
        -- spotify.link share links aren't matched here either, same reasoning as the reddit/tiktok short domains.
        RequestOverride
          { requestOverrideMatch = \r -> NH.host r == "open.spotify.com",
            requestOverrideTransform = oEmbedTransform "https://open.spotify.com/oembed",
            requestOverrideParseTitle = parseOEmbedTitle
          }
      ]
      where
        oEmbedTransform oEmbedEndpoint u =
          Url (oEmbedEndpoint <> "?format=json&url=" <> decodeUtf8 (NUri.urlEncode True (encodeUtf8 (unUrl u))))
        parseOEmbedTitle bs = case A.eitherDecodeStrict bs of
          Right (OEmbedResponse t) -> Right t
          Left err -> Left err
