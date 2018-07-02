module BMark where

import Prelude

import App (fetchUrlEnc)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Globals (closest, innerHtml)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.Affjax.Response as AXRes
import Web.DOM (Element, Node)
import Web.DOM.ChildNode (remove) as DCN
import Web.DOM.DOMTokenList (contains, toggle, remove)
import Web.DOM.Element (fromNode, removeAttribute, setAttribute, toChildNode, toParentNode)
import Web.DOM.Node (fromEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (Event, preventDefault, target)
import Web.HTML (window)
import Web.HTML.HTMLDocument (fromNode, body) as HD
import Web.HTML.HTMLElement (classList, fromNode, fromElement, toNode) as HE
import Web.HTML.Window (document)

maybeElement :: forall f. Applicative f => Maybe Node -> f (Maybe Element)
maybeElement = pure <<< (fromNode =<< _)

deleteAskE :: Event -> Effect Unit
deleteAskE e = do
  preventDefault e
  maybe (pure unit) (toggleDelete true) (fromEventTarget =<< target e)

deleteCancelE :: Event -> Effect Unit
deleteCancelE e = do
  preventDefault e
  maybe (pure unit) (toggleDelete false) (fromEventTarget =<< target e)

toggleDelete :: Boolean -> Node -> Effect Unit
toggleDelete toggle n = void $ runMaybeT $ do
  dl <- MaybeT $ maybeElement =<< closest ".delete_link" n
  c <- MaybeT $ querySelector (QuerySelector ".confirm") (toParentNode dl)
  d <- MaybeT $ querySelector (QuerySelector ".delete") (toParentNode dl)
  MaybeT $ map pure $ removeAttribute "hidden" (if toggle then c else d)
  MaybeT $ map pure $ setAttribute "hidden" "hidden" (if not toggle then c else d)


destroyE :: Event -> Int -> Boolean -> Effect Unit
destroyE e bid isPopup = do
  preventDefault e
  void $ launchAff $ do
    void $ destroy bid
    liftEffect $ runMaybeT $ do
      if isPopup
        then removeBody unit
        else removeBookmarkNode unit
  where
    removeBody _ = do
      bdy <- MaybeT $ HD.body =<< document =<< window 
      MaybeT $ map pure $
        innerHtml bdy "<p class=\'error\'>you killed this bookmark</p>"
    removeBookmarkNode _ = do
      eNode <- MaybeT $ pure $ fromEventTarget =<< target e
      bm <- MaybeT $ maybeElement =<< closest ".bookmark" eNode
      MaybeT $ map pure $ DCN.remove (toChildNode bm)

destroy :: Int -> Aff (AffjaxResponse Unit)
destroy bid =
  fetchUrlEnc DELETE ("bm/" <> show bid) Nothing AXRes.ignore


markReadE :: Event -> Int -> Effect Unit
markReadE e bid = do
  preventDefault e
  void $ launchAff $ do
    void $ markRead bid
    liftEffect $ runMaybeT $ do
      eNode <- MaybeT $ pure $ fromEventTarget =<< target e
      removeUnreadClass eNode
      removeReadNode eNode
  where
    removeUnreadClass eNode = do
      bm <- MaybeT $ maybeElement =<< closest ".bookmark" eNode 
      bt <- MaybeT $ querySelector (QuerySelector ".bookmark_title") (toParentNode bm)
      bte <- MaybeT $ pure $ HE.fromElement bt
      bteClasses <- MaybeT $ map pure $ HE.classList bte
      MaybeT $ map pure $ remove bteClasses "unread"
    removeReadNode eNode = do
      rd <- MaybeT $ maybeElement =<< closest ".mark_read" eNode 
      MaybeT $ map pure $ DCN.remove (toChildNode rd)

markRead :: Int -> Aff (AffjaxResponse Unit)
markRead bid = do
  let path = "bm/" <> show bid <> "/read"
  fetchUrlEnc POST path Nothing AXRes.ignore

data StarAction = Star | UnStar
instance showStar :: Show StarAction where
  show Star = "star"
  show UnStar = "unstar"

toggleStarE :: Event -> Int -> Effect Unit
toggleStarE e bid = do
  preventDefault e
  mstarEl <- runMaybeT $ do
    eNode <- MaybeT $ pure $ fromEventTarget =<< target e
    starNode <- MaybeT $ closest ".star" eNode 
    MaybeT $ pure $ HE.fromNode starNode
  case mstarEl of
    Nothing -> pure unit
    Just starEl -> do
      starClassList <- HE.classList starEl
      let selected_star = "selected_star"
      hasSelectedStar <- starClassList `contains` selected_star
      void $ launchAff $ toggleStar bid (if hasSelectedStar then UnStar else Star)
      void $ toggle starClassList selected_star

toggleStar :: Int -> StarAction -> Aff Unit
toggleStar bid action = do
  let path = "bm/" <> show bid <> "/" <> show action
  void $ fetchUrlEnc POST path Nothing AXRes.ignore
