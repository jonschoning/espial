module Main where

import Prelude

import App (logout)
import Component.Add (addbmark)
import Component.BList (blist)
import Component.NList (nlist)
import Component.NNote (nnote)
import Component.AccountSettings (usetting)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Bookmark, Note, AccountSettings)
import Web.DOM.Element (setAttribute)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML.HTMLElement (toElement)

foreign import _mainImpl :: Effect Unit

main :: Effect Unit
main = _mainImpl

logoutE :: Event -> Effect Unit
logoutE e = void <<< launchAff <<< logout =<< preventDefault e

renderBookmarks :: String -> Array Bookmark -> Effect Unit
renderBookmarks renderElSelector bmarks = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (blist bmarks) unit el
      viewRendered

renderAddForm :: String -> Bookmark -> Effect Unit
renderAddForm renderElSelector bmark = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (addbmark bmark) unit el
      viewRendered

renderNotes :: String -> Array Note -> Effect Unit
renderNotes renderElSelector notes = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (nlist notes) unit el
      viewRendered

renderNote :: String -> Note -> Effect Unit
renderNote renderElSelector note = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (nnote note) unit el
      viewRendered

renderAccountSettings :: String -> AccountSettings -> Effect Unit
renderAccountSettings renderElSelector accountSettings = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (usetting accountSettings) unit el
      viewRendered

viewRendered :: Aff Unit
viewRendered = HA.selectElement (QuerySelector "#content") >>= traverse_ \el ->
  liftEffect $ setAttribute "view-rendered" "" (toElement el) 
