module Handler.Add where

import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Attoparsec.ByteString qualified as AP
import Data.ByteString.Lazy qualified as LBS
import Data.Char (ord)
import Data.Function ((&))
import Data.List (nub)
import Data.Text qualified as T (replace)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Handler.Archive
import Handler.Common (browserUserAgent)
import Import
import Network.HTTP.Client qualified as NH
import Network.HTTP.Client.TLS qualified as NHT

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
      app.dat.bmark = #{ toRawJs bookmarkForm }; 
      app.dat.archiveBackendEnabled = #{ archiveBackendEnabled };
      app.dat.suggestTags = #{ userSuggestTags user };
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderAddForm } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        renderAddForm('##{renderEl}')(app.dat.bmark)();
    |]
  where
    toRawJs = rawJS . decodeUtf8 . encodingToLazyByteString . toEncoding
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
  bookmarkForm <- requireCheckJsonBody
  handleFormSuccess bookmarkForm >>= \case
    Created bid -> sendStatusJSON created201 bid
    Updated _ -> sendResponseStatus noContent204 ()
    Failed s -> sendResponseStatus status400 s
  where
    handleFormSuccess :: BookmarkForm -> Handler (UpsertResult (Key Bookmark))
    handleFormSuccess bookmarkForm = do
      (userId, user) <- requireAuthPair
      appSettings <- appSettings <$> getYesod
      case (appAllowNonHttpUrlSchemes appSettings, (parseRequest . unpack . _url) bookmarkForm) of
        (False, Nothing) -> pure $ Failed "Invalid URL"
        _ -> do
          let mkbid = toSqlKey <$> _bid bookmarkForm
              tags = maybe [] toTagList (_tags bookmarkForm)
          bm <- liftIO $ bookmarkFormToBookmark userId bookmarkForm
          res <- runDB (upsertBookmark userId mkbid bm tags)
          case res of
            Created kbid
              | (maybe (userArchiveDefault user) id (_archiveRequested bookmarkForm)) ->
                  whenM
                    (shouldArchiveBookmark bm)
                    (void $ async $ archiveBookmarkUrl kbid (Url (bookmarkHref bm)))
            _ -> pure ()
          pure res
      where
        toTagList = nub . words . T.replace "," " "

postLookupTitleR :: Handler ()
postLookupTitleR = do
  void requireAuthId
  bookmarkForm <- (requireCheckJsonBody :: Handler BookmarkForm)
  fetchPageTitle (Url (_url bookmarkForm)) >>= \case
    Left _ -> sendResponseStatus noContent204 ()
    Right title -> sendResponseStatus ok200 title
  where
    fetchPageTitle :: Url -> Handler (Either String Text)
    fetchPageTitle url =
      do
        req <- buildPageTitleRequest
        res <- liftIO $ NH.httpLbs req =<< NHT.getGlobalManager
        let body = LBS.toStrict (responseBody res)
        pure (decodeHtmlBs <$> parseTitle body)
        `catch` ( \(e :: SomeException) -> do
                    $(logError) $ (pack . show) e
                    pure (Left (show e))
                )
      where
        parseTitle bs =
          flip AP.parseOnly bs do
            _ <- skipAnyTill (AP.string "<title")
            _ <- skipAnyTill (AP.string ">")
            let lt = toEnum (ord '<')
            AP.takeTill (== lt)
        decodeHtmlBs = toStrict . toLazyText . htmlEncodedText . decodeUtf8
        skipAnyTill end = go where go = end $> () <|> AP.anyWord8 *> go
        buildPageTitleRequest = do
          let UserAgent ua = browserUserAgent
          pure $ NH.parseRequest_ (unpack (unUrl url)) & \r ->
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
