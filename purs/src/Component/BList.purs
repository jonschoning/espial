module Component.BList where

import Prelude

import Component.BMark (BMessage(..), BQuery, bmark)
import Model (Bookmark, BookmarkId)

import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type BSlot = BookmarkId

data LQuery a =
  HandleBMessage BSlot BMessage a

blist :: Array Bookmark -> H.Component HH.HTML LQuery Unit Void Aff
blist st =
  H.parentComponent
    { initialState: const st
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: Array Bookmark -> H.ParentHTML LQuery BQuery BSlot Aff
  render bms =
    HH.div_ (map renderBookmark bms)
    where 
      renderBookmark :: Bookmark -> H.ParentHTML LQuery BQuery BSlot Aff
      renderBookmark b =
        HH.slot
          b.bid
          (bmark b)
          unit
          (HE.input (HandleBMessage b.bid))

  eval :: LQuery ~> H.ParentDSL (Array Bookmark) LQuery BQuery BSlot Void Aff
  eval (HandleBMessage p BNotifyRemove next) = do
    H.modify_ (removeBookmark p)
    pure next
    where
      removeBookmark :: BookmarkId -> Array Bookmark -> Array Bookmark
      removeBookmark bookmarkId = filter (\b -> b.bid /= bookmarkId)
