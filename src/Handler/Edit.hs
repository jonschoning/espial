{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Edit where

import Data.Text qualified as T
import Database.Persist.Sql
import Import

-- routes

patchStarR :: Int64 -> Handler Html
patchStarR bid = _setSelected bid True

patchUnstarR :: Int64 -> Handler Html
patchUnstarR bid = _setSelected bid False

patchReadR :: Int64 -> Handler Html
patchReadR bid = do
  userId <- requireAuthId
  runDBWrite do
    let k_bid = toSqlKey bid
    _ <- _requireResource userId k_bid
    update k_bid [BookmarkToRead =. False]
  pure ""

deleteDeleteR :: Int64 -> Handler Html
deleteDeleteR bid = do
  userId <- requireAuthId
  runDBWrite do
    let k_bid = toSqlKey bid
    _ <- _requireResource userId k_bid
    delete k_bid
  pure ""

postBmBulkEditR :: Handler ()
postBmBulkEditR = do
  app <- getYesod
  (userId, user) <- requireAuthPair
  lang <- getCurrentLang (LangSourceUser (Just user))
  let t key = appTranslate app lang (I18nKey key)
  bmBulkForm <- requireCheckJsonBody
  result <- runDBWrite $ bookmarksBulkEdit userId bmBulkForm
  case result of
    Left err -> sendResponseStatus status409 (translateBulkEditError t err)
    Right editedCount -> do
      let tsuffix = if editedCount == 1 then "_one" else "_other"
      if (editedCount == _beSelectionCount bmBulkForm)
        then
          setMessage
            $ toHtml
            $ T.replace "{{editedCount}}" (tshow editedCount)
            $ t ("bulkEdit.editedCount" <> tsuffix)
        else
          setMessage
            $ toHtml
            $ T.replace "{{selectionCount}}" (tshow (_beSelectionCount bmBulkForm))
            $ T.replace "{{editedCount}}" (tshow editedCount)
            $ t ("bulkEdit.editedCountMismatch" <> tsuffix)
      sendResponseStatus ok200 (object ["editedCount" .= editedCount])
  where
    translateBulkEditError t = \case
      BulkEditErrorPageMismatch ->
        t "bulkEdit.pageMismatch"

-- common

_setSelected :: Int64 -> Bool -> Handler Html
_setSelected bid selected = do
  userId <- requireAuthId
  runDBWrite do
    let k_bid = toSqlKey bid
    bm <- _requireResource userId k_bid
    update k_bid [BookmarkSelected =. selected]
  pure ""

_requireResource :: UserId -> Key Bookmark -> DBM Handler Bookmark
_requireResource userId k_bid = do
  bmark <- get404 k_bid
  if userId == bookmarkUserId bmark
    then pure bmark
    else notFound
