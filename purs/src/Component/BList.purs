module Component.BList where

import Prelude

import Component.BMark (BMessage(..), BQuery, bmark)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Model (Bookmark, BookmarkId)

type BSlot = BookmarkId

data LQuery a =
  HandleBMessage BSlot BMessage a

blist :: forall m. Array Bookmark -> H.Component HH.HTML LQuery Unit Void m
blist st =
  H.parentComponent
    { initialState: const st
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: Array Bookmark -> H.ParentHTML LQuery BQuery BSlot m
  render bms =
    HH.div_ $
      map renderBookmark bms

  renderBookmark :: Bookmark -> H.ParentHTML LQuery BQuery BSlot m
  renderBookmark b =
    HH.slot
      b.id
      (bmark b)
      unit
      (HE.input (HandleBMessage b.id))

  eval :: LQuery ~> H.ParentDSL (Array Bookmark) LQuery BQuery BSlot Void m
  eval (HandleBMessage p msg next) = do
    case msg of
      BNotifyRemove -> do
        H.modify_ (removeBookmark p)
    pure next

removeBookmark :: BookmarkId -> Array Bookmark -> Array Bookmark
removeBookmark bookmarkId st = filter (\b -> b.id /= bookmarkId) st
