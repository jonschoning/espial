module Note where

import Prelude

import Component.NList (nlist)
import Component.NNote (nnote)
import Data.Foldable (traverse_)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Note)
import Web.DOM.ParentNode (QuerySelector(..))
import ViewRendered (viewRendered)

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