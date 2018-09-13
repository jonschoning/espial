module User where

import Prelude

import Component.Add (addbmark)
import Component.BList (blist)
import Component.NList (nlist, nnote)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Bookmark, NoteId, Note)
import Web.DOM.Element (removeAttribute)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (toElement)

renderBookmarks :: String -> Array Bookmark -> Effect Unit
renderBookmarks renderElSelector bmarks = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (blist bmarks) unit el
      showFooter

renderAddForm :: String -> Bookmark -> Effect Unit
renderAddForm renderElSelector bmark = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      runUI (addbmark bmark) unit el

renderNotes :: String -> Array Note -> Effect Unit
renderNotes renderElSelector notes = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (nlist notes) unit el
      showFooter

renderNote :: String -> Note -> Effect Unit
renderNote renderElSelector note = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (nnote note) unit el

showFooter :: Aff Unit
showFooter = HA.selectElement (QuerySelector ".user_footer") >>= traverse_ \el ->
  liftEffect $ removeAttribute "hidden" (toElement el)
