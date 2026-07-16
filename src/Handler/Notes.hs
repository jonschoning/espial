{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Notes where

import Data.Aeson qualified as A
import Data.Text qualified as T
import Handler.Common
import Import
import Network.Wai.Internal qualified as W
import Text.Blaze.Html5 qualified as H
import Yesod.RssFeed

getNotesR :: UserNameP -> Handler Html
getNotesR unamep = _getNotes unamep SharedAll

getNotesSharedR :: UserNameP -> SharedP -> Handler Html
getNotesSharedR = _getNotes

_getNotes :: UserNameP -> SharedP -> Handler Html
_getNotes unamep@(UserNameP uname) sharedp' = do
  app <- getYesod
  muser <- fmap entityVal <$> maybeAuth
  lang <- getCurrentLang (LangSourceUser muser)
  let previewNotes = maybe True userPreviewNotes muser
      frontendBundleName = appFrontendBundleName app
      t = \key -> appTranslate app lang (I18nKey key)
      tc = \key n ->
        let suffix = if n == (1 :: Int) then "_one" else "_other"
         in T.replace "{{count}}" (tshow n) (t (key <> suffix))
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams (Just "n")
  let queryp = "query"
      beforep = pagingCursorBeforeParam
      afterp = pagingCursorAfterParam
      sortp = sortParam
      orderp = orderParam
  mquery <- lookupGetParam queryp
  msort <- lookupGetParam sortp
  morder <- lookupGetParam orderp
  mcursor <-
    parsePagingCursorParams
      (fmap PagingCursorBefore . parsePagingCursorNt)
      (fmap PagingCursorAfter . parsePagingCursorNt)
      <$> lookupGetParam beforep
      <*> lookupGetParam afterp
  let limit = maybe 20 (min 160 . fromIntegral) limit'
      page = maybe 1 fromIntegral page'
      nsort = parseNoteSortParams msort morder
      paging = mkNotePaging nsort mcursor page
      mqueryp = fmap (queryp,) mquery
      msortp = fmap (sortp,) msort
      morderp = fmap (orderp,) morder
      isowner = Just uname == mauthuname
      sharedp = if isowner then sharedp' else SharedPublic
  (bcount, notes, hasEarlier, hasLater) <- runDB do
    Entity userId user <- getBy404 (UniqueUserName uname)
    when
      (not isowner && userPrivacyLock user)
      (redirect (AuthR LoginR))
    getNoteList userId mquery sharedp paging limit
  let mfirstNote = headMay notes
      mlastNote = lastMay notes
      -- the earlier link anchors at the oldest row on the page and the later
      -- link at the newest; which list end holds which depends on direction
      (moldestNote, mnewestNote) = case nsort of
        NoteSort NoteSortCreated SortAsc -> (mfirstNote, mlastNote)
        _ -> (mlastNote, mfirstNote)
      mqueryEarlierp =
        fmap
          (beforep,)
          (formatEntityPagingCursorNt <$> moldestNote)
      mqueryLaterp =
        fmap
          (afterp,)
          (formatEntityPagingCursorNt <$> mnewestNote)
  req <- getRequest
  mroute <- getCurrentRoute
  defaultLayout do
    rssLink (NotesFeedR unamep) "feed"
    let pager = $(widgetFile "pager")
        search = $(widgetFile "search")
        renderEl = "notes" :: Text
    $(widgetFile "notes")
    toWidgetBody
      [julius|
        app.userR = "@{UserR unamep}";
        app.dat.notes = #{ toJSON notes } || [];
        app.dat.bcount = #{ toJSON bcount };
        app.dat.isowner = #{ isowner };
        app.dat.previewNotes = #{ previewNotes };
        app.dat.sharedp = #{ toJSON sharedp };
        app.dat.query = #{ toJSON mquery };
        app.dat.sort = #{ toJSON nsort };
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderNotes, renderNoteBulkEdit } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        renderNotes('##{renderEl}')(app.dat.notes)();
        $if isowner
          renderNoteBulkEdit('#bulkEditRenderEl')(app.dat.bcount)();
    |]

getNoteR :: UserNameP -> NtSlug -> Handler Html
getNoteR unamep@(UserNameP uname) slug = do
  frontendBundleName <- appFrontendBundleName <$> getYesod
  mauthuname <- maybeAuthUsername
  let renderEl = "note" :: Text
      isowner = Just uname == mauthuname
  note <-
    runDB
      $ do
        Entity userId user <- getBy404 (UniqueUserName uname)
        mnote <- getNote userId slug
        note <- maybe notFound pure mnote
        when
          (not isowner && (userPrivacyLock user || (not . noteShared . entityVal) note))
          (redirect (AuthR LoginR))
        pure note
  defaultLayout do
    $(widgetFile "note")
    toWidgetBody
      [julius|
        app.userR = "@{UserR unamep}";
        app.dat.note = #{ toJSON note } || [];
        app.dat.isowner = #{ isowner };
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderNote } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        renderNote('##{renderEl}')(app.dat.note)();
    |]

getAddNoteSlimViewR :: Handler Html
getAddNoteSlimViewR = do
  Entity userId user <- requireAuth
  getAddNoteViewR (UserNameP (userName user))

getAddNoteViewR :: UserNameP -> Handler Html
getAddNoteViewR unamep@(UserNameP uname) = do
  frontendBundleName <- appFrontendBundleName <$> getYesod
  userId <- requireAuthId
  note <- liftIO . _toNote userId =<< noteFormUrl
  let renderEl = "note" :: Text
      enote = Entity (NoteKey 0) note
  defaultLayout do
    $(widgetFile "note")
    toWidgetBody
      [julius|
        app.userR = "@{UserR unamep}";
        app.noteR = "@{NoteR unamep (noteSlug (entityVal enote))}";
        app.dat.note = #{ toJSON enote } || [];
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderNote } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        renderNote('##{renderEl}')(app.dat.note)();
    |]

postNoteBulkEditR :: Handler ()
postNoteBulkEditR = do
  app <- getYesod
  (userId, user) <- requireAuthPair
  lang <- getCurrentLang (LangSourceUser (Just user))
  let t key = appTranslate app lang (I18nKey key)
  noteBulkForm <- requireCheckJsonBody
  result <- runDBWrite $ notesBulkEdit userId noteBulkForm
  case result of
    Left err -> sendResponseStatus status409 (translateBulkEditError t err)
    Right editedCount -> do
      let tsuffix = if editedCount == 1 then "_one" else "_other"
      if (editedCount == _nbeSelectionCount noteBulkForm)
        then
          setMessage
            $ toHtml
            $ T.replace "{{editedCount}}" (tshow editedCount)
            $ t ("bulkEditNotes.editedCount" <> tsuffix)
        else
          setMessage
            $ toHtml
            $ T.replace "{{selectionCount}}" (tshow (_nbeSelectionCount noteBulkForm))
            $ T.replace "{{editedCount}}" (tshow editedCount)
            $ t ("bulkEditNotes.editedCountMismatch" <> tsuffix)
      sendResponseStatus ok200 (object ["editedCount" .= editedCount])
  where
    translateBulkEditError t = \case
      BulkEditErrorPageMismatch ->
        t "bulkEditNotes.pageMismatch"

deleteDeleteNoteR :: Int64 -> Handler Html
deleteDeleteNoteR nid = do
  userId <- requireAuthId
  runDBWrite do
    let k_nid = toSqlKey nid
    _ <- requireResource userId k_nid
    delete k_nid
  pure ""

postAddNoteR :: Handler Text
postAddNoteR = do
  app <- getYesod
  (_, user) <- requireAuthPair
  lang <- getCurrentLang (LangSourceUser (Just user))
  let t key = appTranslate app lang (I18nKey key)
  noteForm <- requireCheckJsonBody
  _handleFormSuccess noteForm >>= \case
    Created note -> sendStatusJSON created201 note
    Updated note -> sendStatusJSON ok200 note
    Failed reason -> sendResponseStatus status400 (translateFailedReason t reason)
  where
    translateFailedReason :: (Text -> Text) -> FailedReason -> Text
    translateFailedReason t = \case
      ReasonUnauthorized -> t "error.upsertUnauthorized"
      ReasonNotFound -> t "error.upsertNotFound"
      ReasonHrefUsedByOther -> t "error.upsertHrefUsedByOther"
      ReasonConflictWithNewer -> t "error.upsertConflictWithNewer"
      ReasonInvalidInput _ -> t "error.upsertInvalidInput"

requireResource :: UserId -> Key Note -> DBM Handler Note
requireResource userId k_nid = do
  nnote <- get404 k_nid
  if userId == noteUserId nnote
    then pure nnote
    else notFound

_handleFormSuccess :: NoteForm -> Handler (UpsertResult (Entity Note))
_handleFormSuccess noteForm = do
  userId <- requireAuthId
  note <- liftIO $ _toNote userId noteForm
  runDBWrite (upsertNote userId knid note)
  where
    knid = toSqlKey <$> (_id noteForm >>= \i -> if i > 0 then Just i else Nothing)

data NoteForm = NoteForm
  { _id :: Maybe Int64,
    _slug :: Maybe NtSlug,
    _title :: Maybe Text,
    _text :: Maybe Textarea,
    _isMarkdown :: Maybe Bool,
    _shared :: Maybe Bool,
    _created :: Maybe UTCTimeStr,
    _updated :: Maybe UTCTimeStr
  }
  deriving (Show, Eq, Read, Generic)

instance FromJSON NoteForm where parseJSON = A.genericParseJSON gNoteFormOptions

instance ToJSON NoteForm where toJSON = A.genericToJSON gNoteFormOptions

gNoteFormOptions :: A.Options
gNoteFormOptions = A.defaultOptions {A.fieldLabelModifier = drop 1}

noteFormUrl :: Handler NoteForm
noteFormUrl = do
  title <- lookupGetParam "title"
  description <- lookupGetParam "description" <&> fmap Textarea
  isMarkdown <- lookupGetParam "isMarkdown" <&> fmap parseChk
  pure
    $ NoteForm
      { _id = Nothing,
        _slug = Nothing,
        _title = title,
        _text = description,
        _isMarkdown = isMarkdown,
        _shared = Nothing,
        _created = Nothing,
        _updated = Nothing
      }
  where
    parseChk s = s == "yes" || s == "on" || s == "true" || s == "1"

_toNote :: UserId -> NoteForm -> IO Note
_toNote userId NoteForm {..} = do
  time <- liftIO getCurrentTime
  slug <- maybe mkNtSlug pure _slug
  pure
    $ Note
      { noteUserId = userId,
        noteSlug = slug,
        noteLength = maybe 0 (length . unTextarea) _text,
        noteTitle = fromMaybe "" _title,
        noteText = maybe "" unTextarea _text,
        noteIsMarkdown = Just True == _isMarkdown,
        noteShared = Just True == _shared,
        noteCreated = maybe time unUTCTimeStr _created,
        noteUpdated = maybe time unUTCTimeStr _updated
      }

noteToRssEntry :: (Route App -> Text) -> UserNameP -> Entity Note -> FeedEntry Text
noteToRssEntry render usernamep (Entity entryId entry) =
  FeedEntry
    { feedEntryLink = render $ NoteR usernamep (noteSlug entry),
      feedEntryUpdated = noteUpdated entry,
      feedEntryTitle = noteTitle entry,
      feedEntryContent = toHtml (noteText entry),
      feedEntryEnclosure = Nothing,
      feedEntryCategories = []
    }

getNotesFeedR :: UserNameP -> Handler RepRss
getNotesFeedR unamep@(UserNameP uname) = do
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams (Just "n")
  mquery <- lookupGetParam "query"
  msort <- lookupGetParam sortParam
  morder <- lookupGetParam orderParam
  mbefore <- lookupGetParam pagingCursorBeforeParam
  mafter <- lookupGetParam pagingCursorAfterParam
  let mcursor =
        parsePagingCursorParams
          (fmap PagingCursorBefore . parsePagingCursorNt)
          (fmap PagingCursorAfter . parsePagingCursorNt)
          mbefore
          mafter
  let limit = maybe 20 (min 160 . fromIntegral) limit'
      page = maybe 1 fromIntegral page'
      nsort = parseNoteSortParams msort morder
      paging = mkNotePaging nsort mcursor page
      isowner = Just uname == mauthuname
      sharedp = if isowner then SharedAll else SharedPublic
  (_, notes, _, _) <- runDB do
    Entity userId user <- getBy404 (UniqueUserName uname)
    when
      (not isowner && userPrivacyLock user)
      (redirect (AuthR LoginR))
    getNoteList userId mquery sharedp paging limit
  render <- getUrlRender
  let (descr :: Html) = toHtml $ H.text (uname <> " notes")
      entries = map (noteToRssEntry render unamep) notes
  updated <- case maximumMay (map feedEntryUpdated entries) of
    Nothing -> liftIO getCurrentTime
    Just m -> pure m
  (feedLinkSelf, feedLinkHome) <- getFeedLinkSelf
  rssFeedText
    $ Feed
      { feedTitle = uname <> " notes",
        feedLinkSelf = feedLinkSelf,
        feedLinkHome = feedLinkHome,
        feedAuthor = uname,
        feedDescription = descr,
        feedLanguage = "en",
        feedUpdated = updated,
        feedLogo = Nothing,
        feedEntries = entries
      }
  where
    getFeedLinkSelf = do
      request <- getRequest
      render <- getUrlRender
      let rawRequest = reqWaiRequest request
          feedLinkSelf = render HomeR <> (T.drop 1 (decodeUtf8 (W.rawPathInfo rawRequest <> W.rawQueryString rawRequest)))
          feedLinkHome = render (UserR unamep)
      pure (feedLinkSelf, feedLinkHome)
