module Handler.Add where

import Data.Attoparsec.ByteString qualified as AP
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Function ((&))
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Handler.Archive
import Handler.Common (browserUserAgent)
import Import
import Network.HTTP.Client qualified as NH
import Network.HTTP.Client.TLS qualified as NHT
import Network.PrivateAddress (isDisallowedFetchHost)
import Util
import Data.Either (isRight)

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
    _archive kbid bm = whenM
      (shouldArchiveBookmark bm)
      (archiveBookmarkUrl kbid (Url (bookmarkHref bm)))
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
      toArchive <- fmap catMaybes $ forM (zip3 validRess bms validForms) $ \(res, bm, bookmarkForm) ->
        case res of
          Created kbid | fromMaybe (userArchiveDefault user) (_archiveRequested bookmarkForm) -> _toArchive kbid bm
          Updated kbid | isJust (_archiveRequested bookmarkForm) -> _toArchive kbid bm
          _ -> pure Nothing
      unless (null toArchive) (archiveBookmarkUrls toArchive)
      pure (mergeBulkAddResults marked validRess)

    _toArchive :: Key Bookmark -> Bookmark -> HandlerFor App (Maybe (Key Bookmark, Url))
    _toArchive kbid bm = do
      should <- shouldArchiveBookmark bm
      pure (if should then Just (kbid, Url (bookmarkHref bm)) else Nothing)

    -- | Re-interleaves per-item validation failures with the upsert results of the valid subset, restoring the original request order.
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

postLookupTitleR :: Handler ()
postLookupTitleR = do
  void requireAuthId
  bookmarkForm <- (requireCheckJsonBody :: Handler BookmarkForm)
  fetchPageTitle (Url (_url bookmarkForm)) >>= \case
    Left _ -> sendResponseStatus noContent204 ()
    Right title -> sendResponseStatus ok200 title
  where
    maxTitleFetchBytes :: Int
    maxTitleFetchBytes = 1024 * 1024

    fetchPageTitle :: Url -> Handler (Either String Text)
    fetchPageTitle url =
      do
        let req = buildPageTitleRequest
        disallowed <- liftIO $ isDisallowedFetchHost (unpack (decodeUtf8 (NH.host req)))
        if disallowed
          then pure (Left "URL host is not allowed")
          else do
            manager <- liftIO NHT.getGlobalManager
            body <- liftIO $ NH.withResponse req manager (readBoundedBody . NH.responseBody)
            pure (decodeHtmlBs <$> parseTitle body)
        `catch` ( \(e :: SomeException) -> do
                    $(logError) $ (pack . show) e
                    pure (Left (show e))
                )
      where
        readBoundedBody :: NH.BodyReader -> IO ByteString
        readBoundedBody br = BS.concat <$> go 0
          where
            go total
              | total >= maxTitleFetchBytes = pure []
              | otherwise = do
                  chunk <- NH.brRead br
                  if null chunk
                    then pure []
                    else (chunk :) <$> go (total + BS.length chunk)
        parseTitle bs =
          flip AP.parseOnly bs do
            _ <- skipAnyTill (AP.string "<title")
            _ <- skipAnyTill (AP.string ">")
            let lt = toEnum (ord '<')
            AP.takeTill (== lt)
        decodeHtmlBs = toStrict . toLazyText . htmlEncodedText . decodeUtf8
        skipAnyTill end = go where go = end $> () <|> AP.anyWord8 *> go
        buildPageTitleRequest =
          let UserAgent ua = browserUserAgent
           in NH.parseRequest_ (unpack (unUrl url)) & \r ->
                r
                  { NH.requestHeaders =
                      [ ("Cache-Control", "max-age=0"),
                        ("User-Agent", encodeUtf8 ua)
                      ]
                  }

postTagSuggestionsR :: Handler Text
postTagSuggestionsR = do
  userId <- requireAuthId
  tagSuggestionRequest <- requireCheckJsonBody
  let suggestion_limit = 10
  tagSuggestions <- runDB $ getTagSuggestions userId tagSuggestionRequest suggestion_limit
  sendStatusJSON ok200 tagSuggestions
