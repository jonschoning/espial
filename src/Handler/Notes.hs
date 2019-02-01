{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Notes where

import Import
import Handler.Common (lookupPagingParams)
import qualified Data.Aeson as A
import qualified Data.Text as T

getNotesR :: UserNameP -> Handler Html
getNotesR unamep@(UserNameP uname) = do
  void requireAuthId
  (limit', page') <- lookupPagingParams
  let queryp = "query" :: Text
  mquery <- lookupGetParam queryp
  let limit = maybe 20 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      mqueryp = fmap (\q -> (queryp, q)) mquery 
  (bcount, notes) <- 
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       getNoteList userId mquery limit page
  req <- getRequest
  mroute <- getCurrentRoute 
  defaultLayout $ do
    let pager = $(widgetFile "pager")
        search = $(widgetFile "search")
        renderEl = "notes" :: Text
    $(widgetFile "notes")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.dat.notes = #{ toJSON notes } || []; 
    |]
    toWidget [julius|
      PS['Main'].renderNotes('##{rawJS renderEl}')(app.dat.notes)();
    |]

getNoteR :: UserNameP -> NtSlug -> Handler Html
getNoteR unamep@(UserNameP uname) slug = do
  void requireAuthId
  let renderEl = "note" :: Text
  note <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       mnote <- getNote userId slug
       maybe notFound pure mnote
  defaultLayout $ do
    $(widgetFile "note")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.dat.note = #{ toJSON note } || []; 
    |]
    toWidget [julius|
      PS['Main'].renderNote('##{rawJS renderEl}')(app.dat.note)();
    |]

getAddNoteViewR :: UserNameP -> Handler Html
getAddNoteViewR unamep@(UserNameP uname) = do
  userId <- requireAuthId
  let renderEl = "note" :: Text
  note <- liftIO $ Entity (NoteKey 0) <$> _toNote userId (NoteForm Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
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
  runDB (upsertNote knid note)
  where
    knid = NoteKey <$> (_id noteForm >>= \i -> if i > 0 then Just i else Nothing)

data NoteForm = NoteForm
  { _id :: Maybe Int64
  , _slug :: Maybe NtSlug
  , _title :: Maybe Text
  , _text :: Maybe Textarea
  , _isMarkdown :: Maybe Bool
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
      (fromMaybe time (fmap unUTCTimeStr _created))
      (fromMaybe time (fmap unUTCTimeStr _updated))
