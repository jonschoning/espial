module Handler.Add where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Function ((&))
import Data.List (nub)
import qualified Data.Text as T (replace)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
import Handler.Archive
import Handler.Common (espialUserAgent)
import Import
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NHT

-- View

getAddViewR :: Handler Html
getAddViewR = do
  Entity userId user <- requireAuth
  frontendBundleName <- appFrontendBundleName <$> getYesod

  murl <- lookupGetParam "url"
  mBookmarkDb <- runDB (fetchBookmarkByUrl userId murl)
  let mformdb = fmap _toBookmarkForm mBookmarkDb
  formurl <- bookmarkFormUrl

  let renderEl = "addForm" :: Text

  popupLayout do
    toWidget
      [whamlet|
      <div id="#{ renderEl }">
    |]
    toWidgetBody
      [julius|
      app.dat.bmark = #{ toJSON (fromMaybe formurl mformdb) }; 
      app.dat.suggestTags = #{ userSuggestTags user };
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderAddForm } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        renderAddForm('##{renderEl}')(app.dat.bmark)();
    |]

bookmarkFormUrl :: Handler BookmarkForm
bookmarkFormUrl = do
  Entity _ user <- requireAuth
  url <- lookupGetParam "url" <&> fromMaybe ""
  title <- lookupGetParam "title"
  description <- lookupGetParam "description" <&> fmap Textarea
  tags <- lookupGetParam "tags"
  private <- lookupGetParam "private" <&> fmap parseChk <&> (<|> Just (userPrivateDefault user))
  toread <- lookupGetParam "toread" <&> fmap parseChk
  pure
    $ BookmarkForm
      { _url = url,
        _title = title,
        _description = description,
        _tags = tags,
        _private = private,
        _toread = toread,
        _bid = Nothing,
        _slug = Nothing,
        _selected = Nothing,
        _time = Nothing,
        _archiveUrl = Nothing
      }
  where
    parseChk s = s == "yes" || s == "on" || s == "true" || s == "1"

-- API

postAddR :: Handler Text
postAddR = do
  bookmarkForm <- requireCheckJsonBody
  _handleFormSuccess bookmarkForm >>= \case
    Created bid -> sendStatusJSON created201 bid
    Updated _ -> sendResponseStatus noContent204 ()
    Failed s -> sendResponseStatus status400 s

_handleFormSuccess :: BookmarkForm -> Handler (UpsertResult (Key Bookmark))
_handleFormSuccess bookmarkForm = do
  (userId, user) <- requireAuthPair
  appSettings <- appSettings <$> getYesod
  case (appAllowNonHttpUrlSchemes appSettings, (parseRequest . unpack . _url) bookmarkForm) of
    (False, Nothing) -> pure $ Failed "Invalid URL"
    (_, _) -> do
      let mkbid = toSqlKey <$> _bid bookmarkForm
          tags = maybe [] (nub . words . T.replace "," " ") (_tags bookmarkForm)
      bm <- liftIO $ _toBookmark userId bookmarkForm
      res <- runDB (upsertBookmark userId mkbid bm tags)
      forM_ (maybeUpsertResult res) $ \kbid ->
        whenM (shouldArchiveBookmark user kbid)
          $ void
          $ async (archiveBookmarkUrl kbid (Url (bookmarkHref bm)))
      pure res

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
          (UserAgent ua) <- espialUserAgent
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
