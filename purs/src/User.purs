module User where

import Prelude

import App (fetchUrlEnc)
import Component.BList (blist)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (mapMaybe, filter)
import Data.Foldable (for_, traverse_)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), joinWith, length, replaceAll, split, take, trim)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Globals (closest, createFormArray, getDataAttribute, innerHtml, moment8601, setInnerHtml)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Model (Bookmark)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.Affjax.Response as AXRes
import Util (_body, _doc, _fromElement, _fromNode, _getElementById, _mt, _mt_pure, _queryBoth, _queryBoth', _querySelector, _querySelectorAll)
import Web.DOM (Node)
import Web.DOM.ChildNode (remove) as DCN
import Web.DOM.DOMTokenList (contains, toggle, toggleForce, remove, add) as DTL
import Web.DOM.Document (createElement)
import Web.DOM.Element (removeAttribute, setAttribute, setClassName, toChildNode, toNode)
import Web.DOM.Node (appendChild, deepClone, fromEventTarget, insertBefore, setTextContent, textContent)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, preventDefault, target)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (classList, fromNode, toElement) as HE
import Web.HTML.HTMLFormElement (fromNode) as HFE
import Web.HTML.HTMLInputElement (fromElement, setValue, value, setChecked, checked) as HIE
import Web.HTML.HTMLTextAreaElement (fromElement, setValue, value) as HTAE

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

destroy :: Int -> Aff (AffjaxResponse Unit)
destroy bid =
  fetchUrlEnc DELETE ("bm/" <> show bid) Nothing AXRes.ignore

-- markReadE

markReadE :: Event -> Int -> Effect Unit
markReadE e bid = do
  preventDefault e
  void $ launchAff $ do
    void $ markRead bid
    liftEffect $ runMaybeT $ do
      eNode <- _mt_pure $ fromEventTarget =<< target e
      removeUnreadClass eNode
      removeReadNode eNode
  where
    removeUnreadClass eNode = do
      bm <- _mt $ closest ".bookmark" eNode 
      bt <- _mt $ _querySelector ".bookmark_title" (_fromNode bm)
      lift $ HE.classList (_fromElement bt) >>= (_ `DTL.remove` "unread")
    removeReadNode eNode = do
      rd <- _mt $ closest ".mark_read" eNode 
      lift $ DCN.remove (toChildNode (_fromNode rd))

markRead :: Int -> Aff (AffjaxResponse Unit)
markRead bid = do
  let path = "bm/" <> show bid <> "/read"
  fetchUrlEnc POST path Nothing AXRes.ignore

-- toggleStarE
  
data StarAction = Star | UnStar
instance showStar :: Show StarAction where
  show Star = "star"
  show UnStar = "unstar"

toggleStarE :: Event -> Int -> Effect Unit
toggleStarE e bid = do
  preventDefault e
  mstarEl <- runMaybeT $ do
    eNode <- _mt_pure $ fromEventTarget =<< target e
    starNode <- _mt $ closest ".star" eNode 
    _mt_pure $ HE.fromNode starNode
  for_ mstarEl \starEl -> do
    starClassList <- HE.classList starEl
    let selected_star = "selected_star"
    hasSelectedStar <- starClassList `DTL.contains` selected_star
    void $ launchAff $ toggleStar bid (if hasSelectedStar then UnStar else Star)
    void $ DTL.toggle starClassList selected_star

toggleStar :: Int -> StarAction -> Aff Unit
toggleStar bid action = do
  let path = "bm/" <> show bid <> "/" <> show action
  void $ fetchUrlEnc POST path Nothing AXRes.ignore

-- editHideE

editHideE :: Event -> Effect Unit
editHideE e = do
  preventDefault e
  body <- _body 
  showDisplay (HE.toElement body)
  removeEdit (HE.toElement body)
  where
    showDisplay body = do
      xs <- _querySelectorAll ".display" body
      for_ xs $ _fromNode >>> removeAttribute "hidden" 
    removeEdit body = do
      xs <- _querySelectorAll ".edit_bookmark_form.active" body
      for_ xs $ _fromNode >>> (toChildNode >>> DCN.remove) 


editE :: Event -> Int -> Effect Unit
editE e bid = do
  editHideE e
  body <- _body 
  doc <- _doc
  void $ runMaybeT $ do
    t <- _mt_pure $ fromEventTarget =<< target e
    bm <- _mt $ (map >>> map) _fromNode (closest ".bookmark" t)
    d <- _mt $ _querySelector ".display" bm
    el' <- _mt $ _getElementById "edit_bookmark_form_template" doc
    lift $ do
      el <- deepClone (toNode el')
      let ele = _fromNode el

      removeAttribute "id" ele
      removeAttribute "hidden" ele

      HE.classList (_fromElement ele) >>= (_ `DTL.add` "active")

      _querySelector "input[name=bid]" ele >>= \me ->
        for_ (me >>= HIE.fromElement) (HIE.setValue (show bid))

      _queryBoth (Tuple ".url" ele) (Tuple ".url_display" bm) \a b -> do
        url <- textContent (toNode b)
        for_ (HIE.fromElement a) (HIE.setValue (trim url))

      _queryBoth (Tuple ".title" ele) (Tuple ".bookmark_title" bm) \a b -> do
        title <- textContent (toNode b)
        for_ (HIE.fromElement a) (HIE.setValue (trim title))

      _queryBoth (Tuple ".description" ele) (Tuple ".description" bm) \a b -> do
        descr <- innerHtml (_fromElement b)
        let descr' = trim $ replaceAll (Pattern "<br>") (Replacement "\n") descr
        for_ (HTAE.fromElement a) (HTAE.setValue descr')

      _queryBoth' (Tuple ".tags" ele) (Tuple ".tag" bm) \a bs -> do
        tags <- traverse textContent bs
        for_ (HIE.fromElement a) (HIE.setValue (joinWith " " tags))

      _querySelector "input[name=private]" ele >>= \me -> do
        isPrivate <- HE.classList (_fromElement bm) >>= (_ `DTL.contains` "private")
        for_ (me >>= HIE.fromElement) (HIE.setChecked isPrivate)

      _queryBoth (Tuple "input[name=toread]" ele) (Tuple ".read .mark_read" bm) \a _ -> do
        for_ (HIE.fromElement a) (HIE.setChecked true)

      setAttribute "hidden" "hidden" d
      void $ insertBefore el (toNode d) (toNode bm)

editSubmitE :: Event -> Effect Unit
editSubmitE e = do
  preventDefault e
  body <- _body 
  doc <- _doc
  void $ runMaybeT $ do
    f <- _mt_pure $ fromEventTarget =<< target e
    let fe = _fromNode f
    fe' <- _mt_pure $ HFE.fromNode f
    bm <- _mt $ (map >>> map) _fromNode (closest ".bookmark" f)
    d <- _mt $ _querySelector ".display" bm
  
    lift $ launchAff $ do
      let dat = Just $ FormURLEncoded $ mapMaybe _mpair (createFormArray fe')
      void $ fetchUrlEnc POST "add?inline=true" dat AXRes.ignore
      liftEffect $ do

        toread <- _querySelector "input[name=toread]" fe >>= \me -> do
          for (me >>= HIE.fromElement) HIE.checked >>= fromMaybe false >>> pure

        private <- _querySelector "input[name=private]" fe >>= \me -> do
          for (me >>= HIE.fromElement) HIE.checked >>= fromMaybe false >>> pure

        _queryBoth (Tuple ".url" fe) (Tuple ".url_display" d) \a b -> do
          url <- for (HIE.fromElement a) HIE.value >>= fromMaybe "" >>> pure
          setTextContent url (toNode b) 

        _queryBoth (Tuple ".title" fe) (Tuple ".bookmark_title" d) \a b -> do
          title <- for (HIE.fromElement a) HIE.value >>= fromMaybe "" >>> pure
          setTextContent title (toNode b) 
          void $ HE.classList (_fromElement b) >>= \cl -> DTL.toggleForce cl "unread" toread
  
        _queryBoth (Tuple ".description" fe) (Tuple ".description" d) \a b -> do
          descr <- for (HTAE.fromElement a) HTAE.value >>= fromMaybe "" >>> pure
          let descr' = trim $ replaceAll (Pattern "\n") (Replacement "<br>") descr
          void $ setInnerHtml descr' (_fromElement b) 
  
        void $ HE.classList (_fromElement bm) >>= \cl -> DTL.toggleForce cl "private" private

        username <- _querySelector ".banner_username" (HE.toElement body) >>= \me -> do
          join <$> for me (getDataAttribute "username") >>= fromMaybe "" >>> pure

        _queryBoth (Tuple ".tags" fe) (Tuple ".tags" d) \a b -> do
          void $ setInnerHtml "" (_fromElement b)
          tags <- for (HIE.fromElement a) HIE.value >>= fromMaybe "" >>> pure
          for_ (words tags) \w -> do
            tag <- createElement "a" (toDocument doc)
            setClassName ("tag" <> (if take 1 w == "." then " private" else "")) tag
            setAttribute "href" ("/u:" <> username <> "/t:" <> w) tag
            setTextContent w (toNode tag)
            appendChild (toNode tag) (toNode b)

        _querySelector ".read" d >>= \me -> do
          for_ me \m -> do
            void $ setInnerHtml "" (_fromElement m)
            when toread $ do
              bid <- getDataAttribute "bid" bm >>= fromMaybe "" >>> pure
              log (bid)
              el <- createElement "a" (toDocument doc)
              setClassName "mark_read" el
              setAttribute "href" "#" el
              setAttribute "onclick" ("PS['User'].markReadE(event)(" <> bid <> ")();") el
              setTextContent "mark as read" (toNode el)
              void $ appendChild (toNode el) (toNode m)

        editHideE e
  where
    words = filter ((0 < _) <<< length) <<< split (Pattern " ")
    _mpair = case _ of
      [k, v] -> Just $ Tuple k (Just v)
      _ -> Nothing

renderBookmarks :: Array Bookmark -> Effect Unit
renderBookmarks bmarks = do
  HA.runHalogenAff do
    HA.selectElement (QuerySelector "#bookmarks") >>= traverse_ \el ->
      runUI (blist bmarks) unit el
