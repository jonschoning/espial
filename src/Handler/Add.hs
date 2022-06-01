module Handler.Add where

import Import
import Handler.Archive
import Data.List (nub)
import qualified Data.Text as T (replace)

-- View

getAddViewR :: Handler Html
getAddViewR = do
  userId <- requireAuthId

  murl <- lookupGetParam "url"
  mBookmarkDb <- runDB (fetchBookmarkByUrl userId murl)
  let mformdb = fmap _toBookmarkForm mBookmarkDb 
  formurl <- bookmarkFormUrl

  let renderEl = "addForm" :: Text

  popupLayout do
    toWidget [whamlet|
      <div id="#{ renderEl }">
    |]
    toWidgetBody [julius|
      app.dat.bmark = #{ toJSON (fromMaybe formurl mformdb) }; 
    |]
    toWidget [julius|
      PS.renderAddForm('##{rawJS renderEl}')(app.dat.bmark)();
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
  pure $
    BookmarkForm
    { _url = url
    , _title = title
    , _description = description
    , _tags = tags
    , _private = private
    , _toread = toread
    , _bid = Nothing
    , _slug = Nothing
    , _selected = Nothing
    , _time = Nothing
    , _archiveUrl = Nothing
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
      let mkbid = BookmarkKey <$> _bid bookmarkForm
          tags = maybe [] (nub . words . T.replace "," " ") (_tags bookmarkForm)
      bm <- liftIO $ _toBookmark userId bookmarkForm
      res <- runDB (upsertBookmark userId mkbid bm tags)
      forM_ (maybeUpsertResult res) $ \kbid ->
        whenM (shouldArchiveBookmark user kbid) $
        void $ async (archiveBookmarkUrl kbid (unpack (bookmarkHref bm)))
      pure res

postLookupTitleR :: Handler ()
postLookupTitleR = do
  void requireAuthId
  bookmarkForm <- (requireCheckJsonBody :: Handler BookmarkForm)
  fetchPageTitle (unpack (_url bookmarkForm)) >>= \case
    Left _ -> sendResponseStatus noContent204 ()
    Right title -> sendResponseStatus ok200 title
