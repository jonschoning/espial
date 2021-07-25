{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import qualified Data.Text as T
import           Handler.Common
import           Import
import qualified Text.Blaze.Html5 as H
import           Yesod.RssFeed
import qualified Data.Map as Map

getUserR :: UserNameP -> Handler Html
getUserR uname@(UserNameP name) = do
  _getUser uname SharedAll FilterAll (TagsP [])

getUserSharedR :: UserNameP -> SharedP -> Handler Html
getUserSharedR uname sharedp =
  _getUser uname sharedp FilterAll (TagsP [])

getUserFilterR :: UserNameP -> FilterP -> Handler Html
getUserFilterR uname filterp =
  _getUser uname SharedAll filterp (TagsP [])

getUserTagsR :: UserNameP -> TagsP -> Handler Html
getUserTagsR uname pathtags =
  _getUser uname SharedAll FilterAll pathtags

_getUser :: UserNameP -> SharedP -> FilterP -> TagsP -> Handler Html
_getUser unamep@(UserNameP uname) sharedp' filterp' (TagsP pathtags) = do
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams
  let limit = maybe 120 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      isowner = maybe False (== uname) mauthuname
      sharedp = if isowner then sharedp' else SharedPublic
      filterp = case filterp' of
        FilterSingle _ -> filterp'
        _ -> if isowner then filterp' else FilterAll
      isAll = filterp == FilterAll && sharedp == SharedAll && pathtags == []
      queryp = "query" :: Text
  mquery <- lookupGetParam queryp
  let mqueryp = fmap (\q -> (queryp, q)) mquery
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
        PS['Main'].renderBookmarks('##{rawJS renderEl}')(app.dat.bmarks)();
      }, 0);
      setTimeout(() => {
        PS['Main'].renderTagCloud('##{rawJS tagCloudRenderEl}')(app.tagCloudMode)();
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
bookmarkToRssEntry ((Entity entryId entry), tags) =
  FeedEntry
  { feedEntryLink = bookmarkHref entry
  , feedEntryUpdated = bookmarkTime entry
  , feedEntryTitle = bookmarkDescription entry
  , feedEntryContent = toHtml (bookmarkExtended entry)
  , feedEntryCategories = map (EntryCategory Nothing Nothing) (maybe [] words tags)
  , feedEntryEnclosure = Nothing
  }

getUserFeedR :: UserNameP -> Handler RepRss
getUserFeedR unamep@(UserNameP uname) = do
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams
  let limit = maybe 120 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      queryp = "query" :: Text
      isowner = maybe False (== uname) mauthuname
  mquery <- lookupGetParam queryp
  (_, btmarks) <- runDB $ do
       Entity userId user <- getBy404 (UniqueUserName uname)
       when (not isowner && userPrivacyLock user)
         (redirect (AuthR LoginR))
       bookmarksTagsQuery userId SharedPublic FilterAll [] mquery limit page
  let (descr :: Html) = toHtml $ H.text ("Bookmarks saved by " <> uname)
      entries = map bookmarkToRssEntry btmarks
  updated <- case maximumMay (map feedEntryUpdated entries) of
                Nothing -> liftIO $ getCurrentTime
                Just m ->  return m
  render <- getUrlRender
  rssFeedText $
    Feed
    { feedTitle = "espial " <> uname
    , feedLinkSelf = render (UserFeedR unamep)
    , feedLinkHome = render (UserR unamep)
    , feedAuthor = uname
    , feedDescription = descr
    , feedLanguage = "en"
    , feedUpdated = updated
    , feedLogo = Nothing
    , feedEntries = entries
    }
