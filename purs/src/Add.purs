module Add where

import Prelude

import Component.Add (addbmark)
import Data.Foldable (traverse_)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Bookmark)
import Web.DOM.ParentNode (QuerySelector(..))
import ViewRendered (viewRendered)

renderAddForm :: String -> Bookmark -> Effect Unit
renderAddForm renderElSelector bmark = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (addbmark bmark) unit el
      viewRendered