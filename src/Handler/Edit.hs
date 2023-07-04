{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Edit where

import Database.Persist.Sql

import Import

-- routes

deleteDeleteR :: Int64 -> Handler Html
deleteDeleteR bid = do
  userId <- requireAuthId
  runDB do
    let k_bid = toSqlKey bid
    _ <- requireResource userId k_bid
    delete k_bid
  return ""

postReadR :: Int64 -> Handler Html
postReadR bid = do
  userId <- requireAuthId
  runDB do
    let k_bid = toSqlKey bid
    _ <- requireResource userId k_bid
    update k_bid [BookmarkToRead =. False]
  return ""

postStarR :: Int64 -> Handler Html
postStarR bid = _setSelected bid True

postUnstarR :: Int64 -> Handler Html
postUnstarR bid = _setSelected bid False

-- common

_setSelected :: Int64 -> Bool -> Handler Html
_setSelected bid selected = do
  userId <- requireAuthId
  runDB do
    let k_bid = toSqlKey bid
    bm <- requireResource userId k_bid
    update k_bid [BookmarkSelected =. selected]
  pure ""

requireResource :: UserId -> Key Bookmark -> DBM Handler Bookmark
requireResource userId k_bid = do
  bmark <- get404 k_bid
  if userId == bookmarkUserId bmark
    then return bmark
    else notFound
