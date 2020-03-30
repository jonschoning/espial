{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Notes where

import Import
import Handler.Common (lookupPagingParams)
import qualified Data.Aeson as A
import qualified Data.Text as T
import           Yesod.RssFeed
import qualified Text.Blaze.Html5 as H

getNotesR :: UserNameP -> Handler Html
getNotesR unamep@(UserNameP uname) = do
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams
  let queryp = "query"
  mquery <- lookupGetParam queryp
  let limit = maybe 20 fromIntegral limit'
      page  = maybe 1  fromIntegral page'
      mqueryp = fmap (\q -> (queryp, q)) mquery
      isowner = maybe False (== uname) mauthuname
  (bcount, notes) <- runDB $ do
    Entity userId user <- getBy404 (UniqueUserName uname)
    let sharedp = if isowner then SharedAll else SharedPublic
    when (not isowner && userPrivacyLock user)
      (redirect (AuthR LoginR))
    getNoteList userId mquery sharedp limit page
  req <- getRequest
  mroute <- getCurrentRoute
  defaultLayout $ do
    rssLink (NotesFeedR unamep) "feed"
    let pager = $(widgetFile "pager")
        search = $(widgetFile "search")
        renderEl = "notes" :: Text
    $(widgetFile "notes")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.dat.notes = #{ toJSON notes } || [];
        app.dat.isowner = #{ isowner };
    |]
    toWidget [julius|
      PS['Main'].renderNotes('##{rawJS renderEl}')(app.dat.notes)();
    |]

getNoteR :: UserNameP -> NtSlug -> Handler Html
getNoteR unamep@(UserNameP uname) slug = do
  mauthuname <- maybeAuthUsername
  let renderEl = "note" :: Text
      isowner = maybe False (== uname) mauthuname
  note <-
    runDB $
    do Entity userId user <- getBy404 (UniqueUserName uname)
       mnote <- getNote userId slug
       note <- maybe notFound pure mnote
       when (not isowner && (userPrivacyLock user || (not . noteShared . entityVal) note))
         (redirect (AuthR LoginR))
       pure note
  defaultLayout $ do
    $(widgetFile "note")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.dat.note = #{ toJSON note } || [];
        app.dat.isowner = #{ isowner };
    |]
    toWidget [julius|
      PS['Main'].renderNote('##{rawJS renderEl}')(app.dat.note)();
    |]

getAddNoteViewR :: UserNameP -> Handler Html
getAddNoteViewR unamep@(UserNameP uname) = do
  userId <- requireAuthId
  let renderEl = "note" :: Text
  note <- liftIO $ Entity (NoteKey 0) <$> _toNote userId (NoteForm Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
  defaultLayout $ do
    $(widgetFile "note")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.noteR = "@{NoteR unamep (noteSlug (entityVal note))}";
        app.dat.note = #{ toJSON note } || [];
    |]
    toWidget [julius|
      PS['Main'].renderNote('##{rawJS renderEl}')(app.dat.note)();
    |]

deleteDeleteNoteR :: Int64 -> Handler Html
deleteDeleteNoteR nid = do
  userId <- requireAuthId
  runDB $ do
    let k_nid = NoteKey nid
    _ <- requireResource userId k_nid
    deleteCascade k_nid
  return ""

postAddNoteR :: Handler ()
postAddNoteR = do
  noteForm <- requireCheckJsonBody
  _handleFormSuccess noteForm >>= \case
    (Created, nid) -> sendStatusJSON created201 nid
    (Updated, _) -> sendResponseStatus noContent204 ()

requireResource :: UserId -> Key Note -> DBM Handler Note
requireResource userId k_nid = do
  nnote <- get404 k_nid
  if userId == noteUserId nnote
    then return nnote
    else notFound

_handleFormSuccess :: NoteForm -> Handler (UpsertResult, Key Note)
_handleFormSuccess noteForm = do
  userId <- requireAuthId
  note <- liftIO $ _toNote userId noteForm
  runDB (upsertNote userId knid note)
  where
    knid = NoteKey <$> (_id noteForm >>= \i -> if i > 0 then Just i else Nothing)

data NoteForm = NoteForm
  { _id :: Maybe Int64
  , _slug :: Maybe NtSlug
  , _title :: Maybe Text
  , _text :: Maybe Textarea
  , _isMarkdown :: Maybe Bool
  , _shared :: Maybe Bool
  , _created :: Maybe UTCTimeStr
  , _updated :: Maybe UTCTimeStr
  } deriving (Show, Eq, Read, Generic)

instance FromJSON NoteForm where parseJSON = A.genericParseJSON gNoteFormOptions
instance ToJSON NoteForm where toJSON = A.genericToJSON gNoteFormOptions

gNoteFormOptions :: A.Options
gNoteFormOptions = A.defaultOptions { A.fieldLabelModifier = drop 1 }

_toNote :: UserId -> NoteForm -> IO Note
_toNote userId NoteForm {..} = do
  time <- liftIO getCurrentTime
  slug <- maybe mkNtSlug pure _slug
  pure $
    Note
      userId
      slug
      (length _text)
      (fromMaybe "" _title)
      (maybe "" unTextarea _text)
      (fromMaybe False _isMarkdown)
      (fromMaybe False _shared)
      (fromMaybe time (fmap unUTCTimeStr _created))
      (fromMaybe time (fmap unUTCTimeStr _updated))

noteToRssEntry :: UserNameP -> Entity Note -> FeedEntry (Route App)
noteToRssEntry usernamep (Entity entryId entry) =
  FeedEntry { feedEntryLink = NoteR usernamep (noteSlug entry)
            , feedEntryUpdated = (noteUpdated entry)
            , feedEntryTitle = (noteTitle entry)
            , feedEntryContent =  (toHtml (noteText entry))
            , feedEntryEnclosure = Nothing
            , feedEntryCategories = []
            }

getNotesFeedR :: UserNameP -> Handler RepRss
getNotesFeedR unamep@(UserNameP uname) = do
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams
  mquery <- lookupGetParam "query"
  let limit = maybe 20 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      isowner = maybe False (== uname) mauthuname
  (_, notes) <- runDB $ do
      Entity userId user <- getBy404 (UniqueUserName uname)
      when (not isowner && userPrivacyLock user)
        (redirect (AuthR LoginR))
      getNoteList userId mquery SharedPublic limit page
  let (descr :: Html) = toHtml $ H.text (uname <> " notes")
      entries = map (noteToRssEntry unamep) notes
  updated <- case maximumMay (map feedEntryUpdated entries) of
                Nothing -> liftIO $ getCurrentTime
                Just m ->  return m
  rssFeed $ Feed (uname <> " notes")
    (NotesFeedR unamep)
    (NotesR unamep)
    uname
    descr
    "en"
    updated
    Nothing
    entries
