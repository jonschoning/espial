{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Add where

import Import
import Database.Persist.Sql
import Data.List (nub)

-- View

getAddViewR :: Handler Html
getAddViewR = do
  userId <- requireAuthId

  murl <- lookupGetParam "url"
  mexisting <- runDB (fetchBookmarkByUrl userId murl)
  let mexisting' = fmap _toBookmarkDefs mexisting
  mgetdefs <- mGetBookmarkForm

  let renderEl = "addForm" :: Text

  popupLayout $ do
    toWidget [whamlet|
      <div id="#{ renderEl }">
    |]
    toWidgetBody [julius|
      app.dat.bmark = #{ toJSON (fromMaybe mgetdefs mexisting') }; 
    |]
    toWidget [julius|
      PS['User'].renderAddForm('##{rawJS renderEl}')(app.dat.bmark)();
    |]

mGetBookmarkForm :: Handler BookmarkForm
mGetBookmarkForm =
  BookmarkForm
    <$> (lookupGetParam "url" >>= pure . fromMaybe "")
    <*> (lookupGetParam "title")
    <*> (lookupGetParam "description" >>= pure . fmap Textarea)
    <*> (lookupGetParam "tags")
    <*> (lookupGetParam "private" >>= pure . fmap parseChk)
    <*> (lookupGetParam "toread" >>= pure . fmap parseChk)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
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
  cpprint bookmarkForm
  userId <- requireAuthId
  time <- liftIO getCurrentTime
  runDB $ upsertBookmark kbid (_toBookmark userId time bookmarkForm) tags
  where
    kbid = toSqlKey <$> _bid bookmarkForm
    tags = maybe [] (nub . words) (_tags bookmarkForm)
