{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.Notes where

import Import
import Text.Read

getNotesR :: UserNameP -> Handler Html
getNotesR unamep@(UserNameP uname) = do
  void requireAuthId
  (limit', page') <- _lookupPagingParams
  let limit = maybe 20 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      renderEl = "notes" :: Text
  notes <- 
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       getNoteList userId limit page
  req <- getRequest
  mroute <- getCurrentRoute 
  let bcount = length notes
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

      


_lookupPagingParams :: Handler (Maybe Int64, Maybe Int64)
_lookupPagingParams = do
  cq <- fmap parseMaybe (lookupGetParam "count")
  cs <- fmap parseMaybe (lookupSession "count")
  mapM_ (setSession "count" . (pack . show)) cq
  pq <- fmap parseMaybe (lookupGetParam "page")
  pure (cq <|> cs, pq)
  where
    parseMaybe x = readMaybe . unpack =<< x
