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

  mgetdefs <- aFormToMaybeGetSuccess (mkAddAForm Nothing)

  (formWidget, _) <- generateFormPost $ renderTable $
    mkAddAForm (maybe mgetdefs (Just . toAddDefs) mexisting)

  viewAddWidget
    formWidget
    (mexisting $> $(widgetFile "add-exists-alert"))
    (maybe "url" (const "tags") murl :: Text)
  where
    toAddDefs :: (Entity Bookmark, [Entity BookmarkTag]) -> AddForm
    toAddDefs (Entity bid Bookmark {..}, tags) =
      AddForm
      { url = bookmarkHref
      , title = Just bookmarkDescription
      , description = Just $ Textarea $ bookmarkExtended
      , tags = Just $ unwords $ fmap (bookmarkTagTag . entityVal) tags
      , private = Just $ not bookmarkShared
      , toread = Just $ bookmarkToRead
      , bid = Nothing
      }

postAddR :: Handler Html
postAddR = do
  userId <- requireAuthId

  ((formResult, formWidget), _) <- runFormPost $ renderTable $ mkAddAForm Nothing

  case formResult of
    FormSuccess addForm -> do
      time <- liftIO getCurrentTime
      newBid <- runDB $ upsertDB
        (toSqlKey <$> bid addForm)
        (toBookmark userId time addForm)
        (maybe [] (nub . words) (tags addForm))
      lookupGetParam "next" >>= \case
        Just next -> redirect next
        Nothing -> popupLayout Nothing [whamlet| <div .alert> Add Successful </div> <script> window.close() </script> |]
    _ ->
      lookupGetParam "inline" >>= \case
        Just _ -> fail "invalid form"
        Nothing -> viewAddWidget formWidget Nothing "Tags"

  where
    toBookmark :: UserId -> UTCTime -> AddForm -> Bookmark
    toBookmark userId time AddForm {..} =
      Bookmark userId url
        (fromMaybe "" title)
        (maybe "" unTextarea description)
        time
        (maybe True not private)
        (fromMaybe False toread)
        False
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
    $(widgetFile "bm")
    $(widgetFile "add")

-- AddForm

data AddForm = AddForm
  { url :: Text
  , title :: Maybe Text
  , description :: Maybe Textarea
  , tags :: Maybe Text
  , private :: Maybe Bool
  , toread :: Maybe Bool
  , bid :: Maybe Int64
  } deriving (Show, Eq, Read, Generic)

mkAddAForm :: MonadHandlerForm m => Maybe AddForm -> AForm m AddForm
mkAddAForm defs = do
    AddForm
      <$> areq urlField (textAttrs $ named "url" "URL") (url <$> defs)
      <*> aopt textField (textAttrs $ named "title" "title") (title <$> defs)
      <*> aopt textareaField (textAreaAttrs $ named "description" "description") (description <$> defs)
      <*> aopt textField (textAttrs $ named "tags" "tags") (tags <$> defs)
      <*> aopt checkBoxField (named "private" "private") (private <$> defs)
      <*> aopt checkBoxField (named "toread" "read later") (toread <$> defs)
      <*> aopt hiddenField (named "bid" "") (bid <$> defs)
  where
    textAttrs = attr ("size", "70")
    textAreaAttrs = attrs [("cols", "70"), ("rows", "4")]
