{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Notes where

import Import
import Handler.Common (lookupPagingParams)

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

