{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Notes where

import Import
import Handler.Common (lookupPagingParams)
import Database.Persist.Sql
import qualified Data.Aeson as A

getNotesR :: UserNameP -> Handler Html
getNotesR unamep@(UserNameP uname) = do
  void requireAuthId
  (limit', page') <- lookupPagingParams
  let limit = maybe 20 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      renderEl = "notes" :: Text
  (bcount, notes) <- 
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       getNoteList userId limit page
  req <- getRequest
  mroute <- getCurrentRoute 
  let pager = $(widgetFile "pager")
  defaultLayout $ do
    $(widgetFile "notes")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.dat.notes = #{ toJSON notes } || []; 
    |]
    toWidget [julius|
      PS['User'].renderNotes('##{rawJS renderEl}')(app.dat.notes)();
    |]

getNoteR :: UserNameP -> Int64 -> Handler Html
getNoteR unamep@(UserNameP uname) nid = do
  void requireAuthId
  let renderEl = "note" :: Text
  note <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       mnote <- getNote userId nid
       maybe notFound pure mnote
  defaultLayout $ do
    $(widgetFile "note")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.dat.note = #{ toJSON note } || []; 
    |]
    toWidget [julius|
      PS['User'].renderNote('##{rawJS renderEl}')(app.dat.note)();
    |]

getAddNoteViewR :: UserNameP -> Handler Html
getAddNoteViewR unamep@(UserNameP uname) = do
  userId <- requireAuthId
  time <- liftIO getCurrentTime
  let renderEl = "note" :: Text
  let note = Entity (toSqlKey 0) $ _toNote userId time (NoteForm Nothing Nothing Nothing Nothing Nothing)
  defaultLayout $ do
    $(widgetFile "note")
    toWidgetBody [julius|
        app.userR = "@{UserR unamep}";
        app.dat.note = #{ toJSON note } || []; 
    |]
    toWidget [julius|
      PS['User'].renderNote('##{rawJS renderEl}')(app.dat.note)();
    |]

deleteDeleteNoteR :: Int64 -> Handler Html
deleteDeleteNoteR bid = do
  userId <- requireAuthId
  runDB $ do
    let k_bid = toSqlKey bid
    _ <- requireResource userId k_bid
    deleteCascade k_bid
  return ""

postAddNoteR :: Handler ()
postAddNoteR = do
  noteForm <- requireCheckJsonBody
  _handleFormSuccess noteForm >>= \case
    (Created, bid) -> sendStatusJSON created201 bid
    (Updated, _) -> sendResponseStatus noContent204 ()

requireResource :: UserId -> Key Note -> DBM Handler Note
requireResource userId k_nid = do
  nnote <- get404 k_nid
  if userId == noteUserId nnote
    then return nnote
    else notFound

_handleFormSuccess :: NoteForm -> Handler (UpsertResult, Key Note)
_handleFormSuccess noteForm = do
  cpprint noteForm
  userId <- requireAuthId
  time <- liftIO getCurrentTime
  runDB (upsertNote kbid (_toNote userId time noteForm))
  where
    kbid = toSqlKey <$> (_id noteForm >>= \i -> if i > 0 then Just i else Nothing)

data NoteForm = NoteForm
  { _id :: Maybe Int64
  , _title :: Maybe Text
  , _text :: Maybe Textarea
  , _created :: Maybe UTCTimeStr
  , _updated :: Maybe UTCTimeStr
  } deriving (Show, Eq, Read, Generic)

instance FromJSON NoteForm where parseJSON = A.genericParseJSON gNoteFormOptions
instance ToJSON NoteForm where toJSON = A.genericToJSON gNoteFormOptions

gNoteFormOptions :: A.Options
gNoteFormOptions = A.defaultOptions { A.fieldLabelModifier = drop 1 } 

_toNote :: UserId -> UTCTime -> NoteForm -> Note
_toNote userId time NoteForm {..} =
  Note userId 
    (length _text)
    (fromMaybe "" _title)
    (maybe "" unTextarea _text)
    (fromMaybe time (fmap unUTCTimeStr _created))
    (fromMaybe time (fmap unUTCTimeStr _updated))
