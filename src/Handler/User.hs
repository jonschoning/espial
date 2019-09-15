{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import qualified Data.Text as T
import           Handler.Common (lookupPagingParams)
import           Import
import           Text.Blaze.Html (toHtml)
import qualified Text.Blaze.Html5 as H
import           Yesod.RssFeed

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
  (bcount, bmarks, alltags) <-
    runDB $
    do Entity userId user <- getBy404 (UniqueUserName uname)
       when (not isowner && userPrivacyLock user)
         (redirect (AuthR LoginR))
       (cnt, bm) <- bookmarksQuery userId sharedp filterp pathtags mquery limit page
       tg <- tagsQuery bm
       pure (cnt, bm, tg)
  when (bcount == 0) (case filterp of FilterSingle _ -> notFound; _ -> pure ())
  mroute <- getCurrentRoute
  req <- getRequest
  defaultLayout $ do
    let pager = $(widgetFile "pager")
        search = $(widgetFile "search")
        renderEl = "bookmarks" :: Text
    rssLink (UserFeedR unamep) "feed"
    $(widgetFile "user")
    toWidgetBody [julius|
        app.dat.bmarks = #{ toJSON $ toBookmarkFormList bmarks alltags } || [];
        app.dat.isowner = #{ isowner };
        app.userR = "@{UserR unamep}";
    |]
    toWidget [julius|
      PS['Main'].renderBookmarks('##{rawJS renderEl}')(app.dat.bmarks)();
    |]

bookmarkToRssEntry :: Entity Bookmark -> FeedEntry Text
bookmarkToRssEntry (Entity entryId entry) =
  FeedEntry { feedEntryLink = (bookmarkHref entry)
            , feedEntryUpdated = (bookmarkTime entry)
            , feedEntryTitle = (bookmarkDescription entry)
            , feedEntryContent =  (toHtml (bookmarkExtended entry))
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
  (_, bmarks) <-
    runDB $
    do Entity userId user <- getBy404 (UniqueUserName uname)
       when (not isowner && userPrivacyLock user)
         (redirect (AuthR LoginR))
       bookmarksQuery userId SharedPublic FilterAll [] mquery limit page
  let (descr :: Html) = toHtml $ H.text ("Bookmarks saved by " <> uname)
      entries = map bookmarkToRssEntry bmarks
  updated <- case maximumMay (map feedEntryUpdated entries) of
                Nothing -> liftIO $ getCurrentTime
                Just m ->  return m
  render <- getUrlRender
  rssFeedText $ Feed ("espial " <> uname)
                     (render (UserFeedR unamep))
                     (render (UserR unamep))
                     uname
                     descr
                     "en"
                     updated
                     Nothing
                     entries
