{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Add where

import Import
import Database.Persist.Sql
import Data.List (nub)
import qualified Data.Time.ISO8601 as TI

getAddR :: Handler Html
getAddR = do
  userId <- requireAuthId

  murl <- lookupGetParam "url"
  mexisting <- runDB $ runMaybeT $ do
    bmark <- MaybeT . getBy . UniqueUserHref userId =<< (MaybeT $ pure murl)
    btags <- MaybeT $ Just <$> withTags (entityKey bmark)
    pure (bmark, btags)

  mgetdefs <- aFormToMaybeGetSuccess (mkBookmarkAForm Nothing)

  (formWidget, _) <- generateFormPost $ renderTable $
    mkBookmarkAForm (maybe mgetdefs (Just . toBookmarkDefs) mexisting)

  viewAddWidget
    formWidget
    (mexisting $> $(widgetFile "add-exists-alert"))
    (maybe "url" (const "tags") murl :: Text)
  where
    toBookmarkDefs :: (Entity Bookmark, [Entity BookmarkTag]) -> BookmarkForm
    toBookmarkDefs (Entity bid Bookmark {..}, tags) =
      BookmarkForm
      { _url = bookmarkHref
      , _title = Just bookmarkDescription
      , _description = Just $ Textarea $ bookmarkExtended
      , _tags = Just $ unwords $ fmap (bookmarkTagTag . entityVal) tags
      , _private = Just $ not bookmarkShared
      , _toread = Just $ bookmarkToRead
      , _bid = Nothing
      , _selected = Just $ bookmarkSelected
      , _time = Just $ UTCTimeStr $ bookmarkTime
      }

postAddR :: Handler Html
postAddR = do
  userId <- requireAuthId

  ((formResult, formWidget), _) <- runFormPost $ renderTable $ mkBookmarkAForm Nothing

  case formResult of
    FormSuccess bookmarkForm -> do
      time <- liftIO getCurrentTime
      newBid <- runDB $ upsertDB
        (toSqlKey <$> _bid bookmarkForm)
        (toBookmark userId time bookmarkForm)
        (maybe [] (nub . words) (_tags bookmarkForm))
      lookupGetParam "next" >>= \case
        Just next -> redirect next
        Nothing -> popupLayout Nothing [whamlet| <div .alert> Add Successful </div> <script> window.close() </script> |]
    _ ->
      lookupGetParam "inline" >>= \case
        Just _ -> fail "invalid form"
        Nothing -> viewAddWidget formWidget Nothing "Tags"

  where
    toBookmark :: UserId -> UTCTime -> BookmarkForm -> Bookmark
    toBookmark userId time BookmarkForm {..} =
      Bookmark userId _url
        (fromMaybe "" _title)
        (maybe "" unTextarea _description)
        time
        (maybe True not _private)
        (fromMaybe False _toread)
        (fromMaybe False _selected)
    upsertDB :: Maybe (Key Bookmark) -> Bookmark -> [Text] -> DB (Key Bookmark)
    upsertDB mbid bookmark tags = do
      let userId = bookmarkUserId bookmark
          url = bookmarkHref bookmark
      bid' <- case mbid of
          Just bid -> do
            get bid >>= \case 
              Just bmark -> do
                replace bid bookmark
                update bid [BookmarkSelected =. bookmarkSelected bmark, BookmarkTime =. bookmarkTime bmark]
                deleteWhere [BookmarkTagBookmarkId ==. bid]
                pure bid
              Nothing -> fail "not found"
          Nothing -> do
            getBy (UniqueUserHref userId url) >>= \case
              Just (Entity bid _) -> deleteCascade bid
              _ -> pure ()
            insert bookmark
      forM_ (zip [1 ..] tags) $
        \(i, tag) -> void $ insert $ BookmarkTag userId tag bid' i
      pure bid'

-- add widget

viewAddWidget :: Widget -> Maybe Widget -> Text -> Handler Html
viewAddWidget formWidget mexists focusEl = do
  let submitText = (maybe "add bookmark" (const "update bookmark") mexists) :: Text
  popupLayout mexists $ do
    $(widgetFile "add")

-- BookmarkForm

mkBookmarkAForm :: MonadHandlerForm m => Maybe BookmarkForm -> AForm m BookmarkForm
mkBookmarkAForm defs = do
    BookmarkForm
      <$> areq urlField (textAttrs $ named "url" "URL") (_url <$> defs)
      <*> aopt textField (textAttrs $ named "title" "title") (_title <$> defs)
      <*> aopt textareaField (textAreaAttrs $ named "description" "description") (_description <$> defs)
      <*> aopt textField (textAttrs $ named "tags" "tags") (_tags <$> defs)
      <*> aopt checkBoxField (named "private" "private") (_private <$> defs)
      <*> aopt checkBoxField (named "toread" "read later") (_toread <$> defs)
      <*> aopt hiddenField (named "bid" "") (_bid <$> defs)
      <*> aopt hiddenField (named "selected" "") (_selected <$> defs)
      <*> aopt hiddenField (named "time" "") (_time <$> defs)
  where
    textAttrs = attr ("size", "70")
    textAreaAttrs = attrs [("cols", "70"), ("rows", "4")]
