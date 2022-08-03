{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE TupleSections #-}
module Handler.User where

import qualified Data.Text as T
import           Handler.Common
import           Import
import qualified Text.Blaze.Html5 as H
import           Yesod.RssFeed
import qualified Data.Map as Map
import qualified Network.Wai.Internal as W

getUserR :: UserNameP -> Handler Html
getUserR uname=
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
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams
  let limit = maybe 120 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      isowner = Just uname == mauthuname
      sharedp = if isowner then sharedp' else SharedPublic
      filterp = case filterp' of
        FilterSingle _ -> filterp'
        _ -> if isowner then filterp' else FilterAll
      isAll = filterp == FilterAll && sharedp == SharedAll && null pathtags
      queryp = "query" :: Text
  mquery <- lookupGetParam queryp
  let mqueryp = fmap (queryp,) mquery
  (bcount, btmarks) <- runDB $ do
       Entity userId user <- getBy404 (UniqueUserName uname)
       when (not isowner && userPrivacyLock user)
         (redirect (AuthR LoginR))
       bookmarksTagsQuery userId sharedp filterp pathtags mquery limit page
  when (bcount == 0) (case filterp of FilterSingle _ -> notFound; _ -> pure ())
  mroute <- getCurrentRoute
  tagCloudMode <- getTagCloudMode isowner pathtags
  req <- getRequest
  defaultLayout do
    let pager = $(widgetFile "pager")
        search = $(widgetFile "search")
        renderEl = "bookmarks" :: Text
        tagCloudRenderEl = "tagCloud" :: Text
    rssLink (UserFeedR unamep) "feed"
    $(widgetFile "user")
    toWidgetBody [julius|
        app.dat.bmarks = #{ toJSON $ toBookmarkFormList btmarks } || [];
        app.dat.isowner = #{ isowner };
        app.userR = "@{UserR unamep}";
        app.tagCloudMode = #{ toJSON $ tagCloudMode } || {};
    |]
    toWidget [julius|
      setTimeout(() => {
        PS.renderBookmarks('##{rawJS renderEl}')(app.dat.bmarks)();
      }, 0);
      setTimeout(() => {
        PS.renderTagCloud('##{rawJS tagCloudRenderEl}')(app.tagCloudMode)();
      }, 0);
    |]

-- Form

postUserTagCloudR :: Handler ()
postUserTagCloudR = do
  userId <- requireAuthId
  mode <- requireCheckJsonBody
  _updateTagCloudMode mode
  tc <- runDB $ case mode of
    TagCloudModeTop _ n -> tagCountTop userId n
    TagCloudModeLowerBound _ n -> tagCountLowerBound userId n
    TagCloudModeRelated _ tags ->  tagCountRelated userId tags
    TagCloudModeNone -> notFound
  sendStatusJSON ok200 (Map.fromList tc :: Map.Map Text Int)

postUserTagCloudModeR :: Handler ()
postUserTagCloudModeR = do
  userId <- requireAuthId
  mode <- requireCheckJsonBody
  _updateTagCloudMode mode

_updateTagCloudMode :: TagCloudMode -> Handler ()
_updateTagCloudMode mode =
  case mode of
    TagCloudModeTop _ _ -> setTagCloudMode mode
    TagCloudModeLowerBound _ _ -> setTagCloudMode mode
    TagCloudModeRelated _ _ -> setTagCloudMode mode
    TagCloudModeNone -> notFound

bookmarkToRssEntry :: (Entity Bookmark, Maybe Text) -> FeedEntry Text
bookmarkToRssEntry (Entity entryId entry, tags) =
  FeedEntry
  { feedEntryLink = bookmarkHref entry
  , feedEntryUpdated = bookmarkTime entry
  , feedEntryTitle = bookmarkDescription entry
  , feedEntryContent = toHtml (bookmarkExtended entry)
  , feedEntryCategories = map (EntryCategory Nothing Nothing) (maybe [] words tags)
  , feedEntryEnclosure = Nothing
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
      page  = maybe 1   fromIntegral page'
      isowner = Just uname == mauthuname
      sharedp = if isowner then sharedp' else SharedPublic
      filterp = case filterp' of
        FilterSingle _ -> filterp'
        _ -> if isowner then filterp' else FilterAll
      -- isAll = filterp == FilterAll && sharedp == SharedAll && null pathtags
      queryp = "query" :: Text
  mquery <- lookupGetParam queryp
  (_, btmarks) <- runDB $ do
       Entity userId user <- getBy404 (UniqueUserName uname)
       when (not isowner && userPrivacyLock user)
         (redirect (AuthR LoginR))
       bookmarksTagsQuery userId sharedp filterp pathtags mquery limit page
  let (descr :: Html) = toHtml $ H.text ("Bookmarks saved by " <> uname)
      entries = map bookmarkToRssEntry btmarks
  updated <- case maximumMay (map feedEntryUpdated entries) of
                Nothing -> liftIO getCurrentTime
                Just m ->  return m
  (feedLinkSelf, feedLinkHome) <- getFeedLinkSelf
  rssFeedText $
    Feed
    { feedTitle = "espial " <> uname
    , feedLinkSelf = feedLinkSelf
    , feedLinkHome = feedLinkHome
    , feedAuthor = uname
    , feedDescription = descr
    , feedLanguage = "en"
    , feedUpdated = updated
    , feedLogo = Nothing
    , feedEntries = entries
    }
  where
    getFeedLinkSelf = do
      request <- getRequest
      render <- getUrlRender
      let rawRequest = reqWaiRequest request
          feedLinkSelf = render HomeR <> (T.drop 1 (decodeUtf8 (W.rawPathInfo rawRequest <> W.rawQueryString rawRequest)))
          feedLinkHome = render (UserR unamep)
      pure (feedLinkSelf, feedLinkHome)
