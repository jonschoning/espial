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
  mformdb <- runDB (fmap _toBookmarkForm <$> fetchBookmarkByUrl userId murl)
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
      PS['Main'].renderAddForm('##{rawJS renderEl}')(app.dat.bmark)();
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
    parseChk s = s == "yes" || s == "on"

-- API

postAddR :: Handler ()
postAddR = do
  bookmarkForm <- requireCheckJsonBody
  _handleFormSuccess bookmarkForm >>= \case
    (Created, bid) -> sendStatusJSON created201 bid
    (Updated, _) -> sendResponseStatus noContent204 ()

_handleFormSuccess :: BookmarkForm -> Handler (UpsertResult, Key Bookmark)
_handleFormSuccess bookmarkForm = do
  (userId, user) <- requireAuthPair
  bm <- liftIO $ _toBookmark userId bookmarkForm
  (res, kbid) <- runDB (upsertBookmark userId mkbid bm tags)
  whenM (shouldArchiveBookmark user kbid) $
    void $ async (archiveBookmarkUrl kbid (unpack (bookmarkHref bm)))
  pure (res, kbid)
  where
    mkbid = BookmarkKey <$> _bid bookmarkForm
    tags = maybe [] (nub . words . T.replace "," " ") (_tags bookmarkForm)

postLookupTitleR :: Handler ()
postLookupTitleR = do
  void requireAuthId
  bookmarkForm <- (requireCheckJsonBody :: Handler BookmarkForm)
  fetchPageTitle (unpack (_url bookmarkForm)) >>= \case
    Left _ -> sendResponseStatus noContent204 ()
    Right title -> sendResponseStatus ok200 title
