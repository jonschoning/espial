{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import Import
import Text.Read
import Database.Persist.Sql
import qualified Database.Esqueleto as E
import qualified Data.Time.ISO8601 as TI


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
  (limit', page') <- _lookupPagingParams
  let limit = maybe 120 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      isowner = maybe False (== uname) mauthuname
      sharedp = if isowner then sharedp' else SharedPublic
      filterp = case filterp' of
        FilterSingle _ -> filterp'
        _ -> if isowner then filterp' else FilterAll
  (bcount, bmarks, alltags) <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       (cnt, bm) <- bookmarksQuery userId sharedp filterp pathtags limit page
       tg <- tagsQuery bm
       pure (cnt, bm, tg)
  mroute <- getCurrentRoute 
  let pager = $(widgetFile "pager")
  let toTitle x = if null x then "[no title]" else x
  req <- getRequest
  defaultLayout $ do
    $(widgetFile "bm")
    $(widgetFile "user")

_lookupPagingParams :: Handler (Maybe Int64, Maybe Int64)
_lookupPagingParams = do
  cq <- fmap parseMaybe (lookupGetParam "count")
  cs <- fmap parseMaybe (lookupSession "count")
  mapM_ (setSession "count" . (pack . show)) cq
  pq <- fmap parseMaybe (lookupGetParam "page")
  pure (cq <|> cs, pq)
  where
    parseMaybe x = readMaybe . unpack =<< x
