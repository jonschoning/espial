module Bookmarks where

import Prelude

import Component.TagCloud (tagcloudcomponent)
import Component.BList (blist)
import Data.Foldable (traverse_)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Bookmark, TagCloudMode, tagCloudModeToF)
import Web.DOM.ParentNode (QuerySelector(..))
import ViewRendered (viewRendered)


renderBookmarks :: String -> Array Bookmark -> Effect Unit
renderBookmarks renderElSelector bmarks = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (blist bmarks) unit el
      viewRendered

renderTagCloud :: String -> TagCloudMode -> Effect Unit
renderTagCloud renderElSelector tagCloudMode = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector renderElSelector) >>= traverse_ \el -> do
      void $ runUI (tagcloudcomponent (tagCloudModeToF tagCloudMode)) unit el