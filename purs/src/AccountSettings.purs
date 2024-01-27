module AccountSettings where

import Prelude

import Component.AccountSettings (usetting)
import Data.Foldable (traverse_)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (AccountSettings)
import Web.DOM.ParentNode (QuerySelector(..))
import ViewRendered (viewRendered)


renderAccountSettings :: String -> AccountSettings -> Effect Unit
renderAccountSettings renderElSelector accountSettings = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (usetting accountSettings) unit el
      viewRendered