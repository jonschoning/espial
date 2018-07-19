module User where

import Prelude

import App (destroy)
import Component.BList (blist)
import Globals (closest, getDataAttribute, moment8601, setInnerHtml)
import Model (Bookmark)
import Util (_body, _fromNode, _mt, _mt_pure, _querySelector, _querySelectorAll)

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM (Node)
import Web.DOM.ChildNode (remove) as DCN
import Web.DOM.Element (removeAttribute, setAttribute, toChildNode)
import Web.DOM.Node (fromEventTarget, setTextContent)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault, target)
import Web.HTML.HTMLElement (toElement) as HE

-- replaceIsoTimestamps

replaceIsoTimestamps :: Unit -> Effect Unit
replaceIsoTimestamps u = do
  body <- _body 
  xs <- _querySelectorAll ".js-moment" (HE.toElement body)
  for_ xs \x -> do
    getDataAttribute "iso8601" (_fromNode x) >>= \s' ->
      for_ s' \s -> do
        Tuple rel fmt <- moment8601 s
        setTextContent rel x 
        setAttribute "title" fmt (_fromNode x)
        setAttribute "style" "visibility:visible" (_fromNode x)

-- deleteAskE

deleteAskE :: Event -> Effect Unit
deleteAskE e = do
  preventDefault e
  traverse_ (toggleDelete true) (fromEventTarget =<< target e)

-- deleteCancelE

deleteCancelE :: Event -> Effect Unit
deleteCancelE e = do
  preventDefault e
  traverse_ (toggleDelete false) (fromEventTarget =<< target e)

toggleDelete :: Boolean -> Node -> Effect Unit
toggleDelete toggle n = void $ runMaybeT $ do
  dl <- _mt $ closest ".delete_link" n
  c <- _mt $ _querySelector ".confirm" (_fromNode dl)
  d <- _mt $ _querySelector ".delete" (_fromNode dl)
  lift $ removeAttribute "hidden" (if toggle then c else d)
  lift $ setAttribute "hidden" "hidden" (if not toggle then c else d)


-- destroyE

destroyE :: Event -> Int -> Boolean -> Effect Unit
destroyE e bid isPopup = do
  preventDefault e
  void $ launchAff $ do
    void $ destroy bid
    liftEffect $ 
      if isPopup
        then removeBody unit
        else removeBookmarkNode unit
  where
    removeBody _ = do
      body <- _body 
      void $ setInnerHtml "<p class=\'error\'>you killed this bookmark</p>" body 
    removeBookmarkNode _ =
      void $ runMaybeT $ do
        eNode <- _mt_pure $ fromEventTarget =<< target e
        bm <- _mt $ closest ".bookmark" eNode
        lift $ DCN.remove (toChildNode (_fromNode bm))


renderBookmarks :: Array Bookmark -> Effect Unit
renderBookmarks bmarks = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector "#bookmarks") >>= traverse_ \el ->
      runUI (blist bmarks) unit el
