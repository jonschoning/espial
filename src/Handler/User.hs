-- {-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.User where

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
  lang <- getCurrentLang (LangSourceUser muser)
  let t = \key -> appTranslate app lang (I18nKey key)
      frontendBundleName = appFrontendBundleName app
  (limit', page') <- lookupPagingParams Nothing
  let limit = maybe 120 (min 160 . fromIntegral) limit'
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
      sortp = sortParam
      orderp = orderParam
  mquery <- lookupGetParam queryp
  msort <- lookupGetParam sortp
  morder <- lookupGetParam orderp
  mcursor <-
    parsePagingCursorParams
      (fmap PagingCursorBefore . parsePagingCursorBm)
      (fmap PagingCursorAfter . parsePagingCursorBm)
      <$> lookupGetParam beforep
      <*> lookupGetParam afterp
  let bsort = parseBookmarkSortParams msort morder
      BookmarkSort bsortField bsortDir = bsort
      paging = mkBookmarkPaging bsort mcursor page
      isCursorPaging = case paging of
        PageByCursor {} -> True
        PageByOffset {} -> False
  (suggestTags, suggestTagsUseReturnKey, publicTagCloud, bcount, btmarks, hasPrevious, hasNext) <- runDB $ do
    Entity userId user <- getBy404 (UniqueUserName uname)
    when
      (not isowner && userPrivacyLock user)
      (redirect (AuthR LoginR))
    (bcount, btmarks, hasPrevious, hasNext) <-
      bookmarksTagsQuery userId isowner sharedp filterp pathtags mquery paging limit
    pure (userSuggestTags user, userSuggestTagsUseReturnKey user, userPublicTagCloud user, bcount, btmarks, hasPrevious, hasNext)
  when (bcount == 0) (case filterp of FilterSingle _ -> notFound; _ -> pure ())
  mroute <- getCurrentRoute
  tagCloudMode <- getTagCloudMode isowner publicTagCloud pathtags
  render <- getUrlRender
  let archiveBackendEnabled = isJust (appArchiver app)
      mfirstBookmark = headMay (map fst btmarks)
      mlastBookmark = lastMay (map fst btmarks)
      -- the earlier link anchors at the oldest row on the page and the later
      -- link at the newest; which list end holds which depends on direction
      (moldestBookmark, mnewestBookmark) = case bsort of
        BookmarkSort BookmarkSortTime SortAsc -> (mfirstBookmark, mlastBookmark)
        _ -> (mlastBookmark, mfirstBookmark)
      pagep = pagingPageParam Nothing
      -- offset paging has no cursor entities to anchor on, so it pages by
      -- adjacent page numbers instead of before/after cursors
      mqueryPreviousp
        | isCursorPaging = fmap (beforep,) (formatEntityPagingCursorBm <$> moldestBookmark)
        | otherwise = Just (pagep, tshow (page - 1))
      mqueryNextp
        | isCursorPaging = fmap (afterp,) (formatEntityPagingCursorBm <$> mnewestBookmark)
        | otherwise = Just (pagep, tshow (page + 1))
      mqueryp = fmap (queryp,) mquery
      msortp = fmap (sortp,) msort
      morderp = fmap (orderp,) morder
      renderEl = "bookmarks" :: Text
      showTagCloud = isowner || publicTagCloud
      tagCloudUrl :: Text
      tagCloudUrl
        | isowner = render UserTagCloudR
        | publicTagCloud = render (UserPublicTagCloudR unamep)
        | otherwise = ""
      TagCloudHamletBindings {..} = mkTagCloudHamletBindings tagCloudMode

  defaultLayout do
    let pager = $(widgetFile "pager")
        search = $(widgetFile "search")
    rssLink (UserFeedR unamep) "feed"
    $(widgetFile "user")
    toWidgetBody
      [julius|
        app.dat.bmarks = #{ toJSON $ toBookmarkFormListForViewer isowner btmarks } || [];
        app.dat.bcount = #{ toJSON bcount };
        app.dat.isowner = #{ isowner };
        app.dat.suggestTags = #{ suggestTags };
        app.dat.suggestTagsUseReturnKey = #{ suggestTagsUseReturnKey };
        app.dat.archiveBackendEnabled = #{ archiveBackendEnabled };
        app.dat.filter = #{ toJSON filterp } || {};
        app.dat.sharedp = #{ toJSON sharedp };
        app.dat.tags = #{ toJSON pathtags };
        app.dat.query = #{ toJSON mquery };
        app.dat.sort = #{ toJSON bsort };
        app.userR = "@{UserR unamep}";
        app.tagCloudMode = #{ toJSON $ tagCloudMode } || {};
        app.tagCloudR = #{toJSON tagCloudUrl};
    |]
    toWidget
      [hamlet|
      <script type="module">
        import { renderBookmarks, renderTagCloud, bindTagCloudHeader, renderBulkEdit } from '@{StaticR (StaticRoute ["js", frontendBundleName] [])}'
        setTimeout(() => {
          renderBookmarks('##{renderEl}')(app.dat.bmarks)();
        }, 0);
        setTimeout(() => {
          renderTagCloud('##{tagCloudBodyEl}')(app.tagCloudMode)();
        }, 0);
        setTimeout(() => {
          bindTagCloudHeader('##{tagCloudHeaderEl}')();
        }, 0);
        $if isowner
          setTimeout(() => {
            renderBulkEdit('#bulkEditRenderEl')(app.dat.bcount)();
          }, 0);
    |]

-- | Values the "user" hamlet template needs to render the tag cloud header
-- (mode toggle, min-count filters, expand/collapse), derived from the
-- session/route-resolved 'TagCloudMode'.
data TagCloudHamletBindings = TagCloudHamletBindings
  { tagCloudHeaderEl :: Text,
    tagCloudBodyEl :: Text,
    isTagCloudModeRelated :: Bool,
    tagCloudExpanded :: Bool,
    tagCloudActiveTop :: Bool,
    tagCloudActiveRelated :: Bool,
    tagCloudActiveLowerBound :: Maybe Int,
    tagCloudActiveRelatedLowerBound :: Maybe Int,
    tagCloudCounts :: [(Int, Text)]
  }

mkTagCloudHamletBindings :: TagCloudMode -> TagCloudHamletBindings
mkTagCloudHamletBindings tagCloudMode =
  TagCloudHamletBindings
    { tagCloudHeaderEl = "tagCloudHeader",
      tagCloudBodyEl = "tagCloudBody",
      isTagCloudModeRelated = case tagCloudMode of
        TagCloudModeRelated _ _ -> True
        TagCloudModeRelatedLowerBound _ _ _ -> True
        _ -> False,
      tagCloudExpanded = isExpanded tagCloudMode,
      tagCloudActiveTop = case tagCloudMode of
        TagCloudModeTop _ -> True
        _ -> False,
      tagCloudActiveRelated = case tagCloudMode of
        TagCloudModeRelated _ _ -> True
        _ -> False,
      tagCloudActiveLowerBound = case tagCloudMode of
        TagCloudModeTopLowerBound _ n -> Just n
        _ -> Nothing,
      tagCloudActiveRelatedLowerBound = case tagCloudMode of
        TagCloudModeRelatedLowerBound _ _ n -> Just n
        _ -> Nothing,
      tagCloudCounts =
        [ (1, "tagCloud.allTitle"),
          (2, "tagCloud.min2Title"),
          (5, "tagCloud.min5Title"),
          (10, "tagCloud.min10Title"),
          (20, "tagCloud.min20Title")
        ]
    }

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
  void requireAuthId
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
bookmarkToRssEntry (Entity _entryId entry, tags) =
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
  (limit', page') <- lookupPagingParams Nothing
  let limit = maybe 120 (min 160 . fromIntegral) limit'
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
      sortp = sortParam
      orderp = orderParam
  mquery <- lookupGetParam queryp
  msort <- lookupGetParam sortp
  morder <- lookupGetParam orderp
  mcursor <-
    parsePagingCursorParams
      (fmap PagingCursorBefore . parsePagingCursorBm)
      (fmap PagingCursorAfter . parsePagingCursorBm)
      <$> lookupGetParam beforep
      <*> lookupGetParam afterp
  let bsort = parseBookmarkSortParams msort morder
      paging = mkBookmarkPaging bsort mcursor page
  (_, btmarks, _, _) <- runDB $ do
    Entity userId user <- getBy404 (UniqueUserName uname)
    when
      (not isowner && userPrivacyLock user)
      (redirect (AuthR LoginR))
    bookmarksTagsQuery userId isowner sharedp filterp pathtags mquery paging limit
  let (descr :: Html) = toHtml $ H.text ("Bookmarks saved by " <> uname)
      entries = map bookmarkToRssEntry btmarks
  updated <- case maximumMay (map feedEntryUpdated entries) of
    Nothing -> liftIO getCurrentTime
    Just m -> pure m
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
