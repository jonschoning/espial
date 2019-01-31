{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import Import
import qualified Data.Text as T
import Handler.Common (lookupPagingParams)

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
    $(widgetFile "user")
    toWidgetBody [julius|
        app.dat.bmarks = #{ toJSON $ toBookmarkFormList bmarks alltags } || []; 
        app.dat.isowner = #{ isowner };
        app.userR = "@{UserR unamep}";
    |]
    toWidget [julius|
      PS['Main'].renderBookmarks('##{rawJS renderEl}')(app.dat.bmarks)();
    |]
