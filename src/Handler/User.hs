{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.User where

import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (diffUTCTime)
import Handler.Common
import Import
import Network.Wai.Internal qualified as W
import Text.Blaze.Html5 qualified as H
import Yesod.RssFeed

getUserR :: UserNameP -> Handler Html
getUserR uname =
  _getUser uname SharedAll FilterAll (TagsP [])

getUserSharedR :: UserNameP -> SharedP -> Handler Html
getUserSharedR uname sharedp =
  _getUser uname sharedp FilterAll (TagsP [])

getUserFilterR :: UserNameP -> FilterP -> Handler Html
getUserFilterR uname filterp =
  _getUser uname SharedAll filterp (TagsP [])

getUserTagsR :: UserNameP -> TagsP -> Handler Html
getUserTagsR uname = _getUser uname SharedAll FilterAll

_getUser :: UserNameP -> SharedP -> FilterP -> TagsP -> Handler Html
_getUser unamep@(UserNameP uname) sharedp' filterp' (TagsP pathtags) = do
  app <- getYesod
  muser <- fmap entityVal <$> maybeAuth
  let lang = fromMaybe (appLanguageDefault (appSettings app)) (muser >>= userLanguage)
      t = \key -> appTranslate app lang (I18nKey key)
      frontendBundleName = appFrontendBundleName app
  (limit', page') <- lookupPagingParams
  let limit = maybe 120 fromIntegral limit'
      page = maybe 1 fromIntegral page'
      isowner = Just uname == fmap userName muser
      sharedp = if isowner then sharedp' else SharedPublic
      filterp = case filterp' of
        FilterSingle _ -> filterp'
        _ -> if isowner then filterp' else FilterAll
      isAll = filterp == FilterAll && sharedp == SharedAll && null pathtags
      queryp = "query" :: Text
      beforep = pagingCursorBeforeParam
      afterp = pagingCursorAfterParam
  mquery <- lookupGetParam queryp
  mcursor <-
    parsePagingCursorParams
      (fmap PagingCursorBefore . parsePagingCursorTime)
      (fmap PagingCursorAfter . parsePagingCursorTime)
      <$> lookupGetParam beforep
      <*> lookupGetParam afterp
  (suggestTags, publicTagCloud, bcount, btmarks, hasEarlier, hasLater) <- runDB $ do
    Entity userId user <- getBy404 (UniqueUserName uname)
    when
      (not isowner && userPrivacyLock user)
      (redirect (AuthR LoginR))
    (bcount, btmarks, hasEarlier, hasLater) <-
      bookmarksTagsQuery userId isowner sharedp filterp pathtags mquery mcursor limit page
    pure (userSuggestTags user, userPublicTagCloud user, bcount, btmarks, hasEarlier, hasLater)
  when (bcount == 0) (case filterp of FilterSingle _ -> notFound; _ -> pure ())
  mroute <- getCurrentRoute
  tagCloudMode <- getTagCloudMode isowner publicTagCloud pathtags
  render <- getUrlRender
  let archiveBackendEnabled = isJust (appArchiver app)
      mfirstBookmark = headMay (map fst btmarks)
      mlastBookmark = lastMay (map fst btmarks)
      mqueryEarlierp =
        fmap
          (beforep,)
          (formatEntityPagingCursorTimeBm <$> mlastBookmark)
      mqueryLaterp =
        fmap
          (afterp,)
          (formatEntityPagingCursorTimeBm <$> mfirstBookmark)
      mqueryp = fmap (queryp,) mquery
      renderEl = "bookmarks" :: Text
      tagCloudRenderEl = "tagCloud" :: Text
      showTagCloud = isowner || publicTagCloud
      tagCloudUrl :: Text
      tagCloudUrl
        | isowner = render UserTagCloudR
        | publicTagCloud = render (UserPublicTagCloudR unamep)
        | otherwise = ""

  defaultLayout do
    let pager = $(widgetFile "pager")
        search = $(widgetFile "search")
    rssLink (UserFeedR unamep) "feed"
    $(widgetFile "user")
    toWidgetBody
      [julius|
        app.dat.bmarks = #{ toRawJs $ toBookmarkFormListForViewer isowner btmarks } || [];
        app.dat.bcount = #{ toJSON bcount };
        app.dat.isowner = #{ isowner };
        app.dat.suggestTags = #{ suggestTags };
        app.dat.archiveBackendEnabled = #{ archiveBackendEnabled };
        app.dat.filter = #{ toJSON filterp } || {};
        app.dat.sharedp = #{ toJSON sharedp };
        app.dat.tags = #{ toJSON pathtags };
        app.dat.query = #{ toJSON mquery };
        app.userR = "@{UserR unamep}";
        app.tagCloudMode = #{ toJSON $ tagCloudMode } || {};
        app.tagCloudR = #{toJSON tagCloudUrl};
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderBookmarks, renderTagCloud, renderBulkEdit } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        setTimeout(() => {
          renderBookmarks('##{renderEl}')(app.dat.bmarks)();
        }, 0);
        setTimeout(() => {
          renderTagCloud('##{tagCloudRenderEl}')(app.tagCloudMode)();
        }, 0);
        $if isowner
          setTimeout(() => {
            renderBulkEdit('#bulkEditRenderEl')(app.dat.bcount)();
          }, 0);
    |]
  where
    toRawJs = rawJS . decodeUtf8 . encodingToLazyByteString . toEncoding

-- Form

postUserTagCloudR :: Handler ()
postUserTagCloudR = do
  userId <- requireAuthId
  mode <- requireCheckJsonBody
  _updateTagCloudMode mode
  tc <- runDB $ case mode of
    TagCloudModeTop _ -> tagCountTop userId True
    TagCloudModeTopLowerBound _ n -> tagCountLowerBound userId n True
    TagCloudModeRelated _ tags -> tagCountRelated userId tags True
    TagCloudModeRelatedLowerBound _ tags lb -> tagCountRelatedLowerBound userId tags lb True
    TagCloudModeNone -> pure []
  sendStatusJSON ok200 (Map.fromList tc :: Map.Map Text Int)

postUserPublicTagCloudR :: UserNameP -> Handler ()
postUserPublicTagCloudR (UserNameP uname) = do
  Entity userId user <- runDB $ getBy404 (UniqueUserName uname)
  unless (userPublicTagCloud user) notFound
  when (userPrivacyLock user) notFound
  mode <- requireCheckJsonBody
  _updateTagCloudMode mode
  app' <- getYesod
  let cacheRef = appPublicTagCloudCache app'
      ttl = fromIntegral (appPublicTagCloudCacheDurationSeconds (appSettings app'))
  now <- liftIO getCurrentTime
  cacheMap <- liftIO $ readIORef cacheRef
  case Map.lookup (uname, mode) cacheMap of
    Just (storedAt, tc)
      | diffUTCTime now storedAt < ttl ->
          sendStatusJSON ok200 tc
    _ -> do
      tcList <- runDB $ case mode of
        TagCloudModeTop _ -> tagCountTop userId False
        TagCloudModeTopLowerBound _ n -> tagCountLowerBound userId n False
        TagCloudModeRelated _ tags -> tagCountRelated userId tags False
        TagCloudModeRelatedLowerBound _ tags lb -> tagCountRelatedLowerBound userId tags lb False
        TagCloudModeNone -> pure []
      let tc = Map.fromList tcList
      liftIO $ atomicModifyIORef' cacheRef $ \m ->
        let pruned = Map.filter (\(t, _) -> diffUTCTime now t < ttl) m
            evicted = if Map.size pruned >= 1000 then Map.deleteMin pruned else pruned
         in (Map.insert (uname, mode) (now, tc) evicted, ())
      sendStatusJSON ok200 tc

postUserTagCloudModeR :: Handler ()
postUserTagCloudModeR = do
  userId <- requireAuthId
  mode <- requireCheckJsonBody
  _updateTagCloudMode mode

_updateTagCloudMode :: TagCloudMode -> Handler ()
_updateTagCloudMode mode =
  case mode of
    TagCloudModeTop _ -> setTagCloudMode mode
    TagCloudModeTopLowerBound _ _ -> setTagCloudMode mode
    TagCloudModeRelated _ _ -> pure ()
    TagCloudModeRelatedLowerBound _ _ _ -> pure ()
    TagCloudModeNone -> notFound

bookmarkToRssEntry :: (Entity Bookmark, Maybe Text) -> FeedEntry Text
bookmarkToRssEntry (Entity entryId entry, tags) =
  FeedEntry
    { feedEntryLink = bookmarkHref entry,
      feedEntryUpdated = bookmarkTime entry,
      feedEntryTitle = bookmarkDescription entry,
      feedEntryContent = toHtml (bookmarkExtended entry),
      feedEntryCategories = map (EntryCategory Nothing Nothing) (maybe [] words tags),
      feedEntryEnclosure = Nothing
    }

getUserFeedR :: UserNameP -> Handler RepRss
getUserFeedR unamep = do
  _getUserFeed unamep SharedAll FilterAll (TagsP [])

getUserFeedSharedR :: UserNameP -> SharedP -> Handler RepRss
getUserFeedSharedR uname sharedp =
  _getUserFeed uname sharedp FilterAll (TagsP [])

getUserFeedFilterR :: UserNameP -> FilterP -> Handler RepRss
getUserFeedFilterR uname filterp =
  _getUserFeed uname SharedAll filterp (TagsP [])

getUserFeedTagsR :: UserNameP -> TagsP -> Handler RepRss
getUserFeedTagsR uname = _getUserFeed uname SharedAll FilterAll

_getUserFeed :: UserNameP -> SharedP -> FilterP -> TagsP -> Handler RepRss
_getUserFeed unamep@(UserNameP uname) sharedp' filterp' (TagsP pathtags) = do
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams
  let limit = maybe 120 fromIntegral limit'
      page = maybe 1 fromIntegral page'
      isowner = Just uname == mauthuname
      sharedp = if isowner then sharedp' else SharedPublic
      filterp = case filterp' of
        FilterSingle _ -> filterp'
        _ -> if isowner then filterp' else FilterAll
      -- isAll = filterp == FilterAll && sharedp == SharedAll && null pathtags
      queryp = "query" :: Text
      beforep = pagingCursorBeforeParam
      afterp = pagingCursorAfterParam
  mquery <- lookupGetParam queryp
  mcursor <-
    parsePagingCursorParams
      (fmap PagingCursorBefore . parsePagingCursorTime)
      (fmap PagingCursorAfter . parsePagingCursorTime)
      <$> lookupGetParam beforep
      <*> lookupGetParam afterp
  (_, btmarks, _, _) <- runDB $ do
    Entity userId user <- getBy404 (UniqueUserName uname)
    when
      (not isowner && userPrivacyLock user)
      (redirect (AuthR LoginR))
    bookmarksTagsQuery userId isowner sharedp filterp pathtags mquery mcursor limit page
  let (descr :: Html) = toHtml $ H.text ("Bookmarks saved by " <> uname)
      entries = map bookmarkToRssEntry btmarks
  updated <- case maximumMay (map feedEntryUpdated entries) of
    Nothing -> liftIO getCurrentTime
    Just m -> return m
  (feedLinkSelf, feedLinkHome) <- getFeedLinkSelf
  rssFeedText
    $ Feed
      { feedTitle = "espial " <> uname,
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
