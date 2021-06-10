module Component.BList where

import Prelude

import Component.BMark (BMessage(..), BSlot, bmark)
import Model (Bookmark, BookmarkId)

import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

data LAction =
  HandleBMessage BookmarkId BMessage

type ChildSlots =
  ( bookmark :: BSlot Int
  )

_bookmark = Proxy :: Proxy "bookmark"

blist :: forall q i o. Array Bookmark -> H.Component q i o Aff
blist st =
  H.mkComponent
    { initialState: const st
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  render :: Array Bookmark -> H.ComponentHTML LAction ChildSlots Aff
  render bms =
    HH.div_ $ map (\b -> HH.slot _bookmark b.bid (bmark b) unit (HandleBMessage b.bid)) bms

  handleAction :: LAction -> H.HalogenM (Array Bookmark) LAction ChildSlots o Aff Unit
  handleAction (HandleBMessage bid BNotifyRemove) = do
    H.modify_ (filter (\b -> b.bid /= bid))
