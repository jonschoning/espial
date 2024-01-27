module ViewRendered where

import Prelude

import Data.Foldable (traverse_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Web.DOM.Element (setAttribute)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (toElement)

viewRendered :: Aff Unit
viewRendered = HA.selectElement (QuerySelector "#content") >>= traverse_ \el ->
  liftEffect $ setAttribute "view-rendered" "" (toElement el) 
