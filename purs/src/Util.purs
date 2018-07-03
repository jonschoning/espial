module Util where

import Prelude
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Element, Node)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (fromNode, toParentNode)
import Web.DOM.NodeList (toArray)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (body) as HD
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement (fromElement) as HE
import Web.HTML.Window (document)
  
-- Util

_queryBoth :: forall a. Tuple String Element -> Tuple String Element -> (Element -> Element -> Effect a) -> Effect Unit
_queryBoth (Tuple qa ea) (Tuple qb eb) f = do
   ma <- _querySelector qa ea
   mb <- _querySelector qb eb
   for_ ma \a -> 
     for_ mb \b ->
       f a b

_queryBoth' :: forall a. Tuple String Element -> Tuple String Element -> (Element -> Array Node -> Effect a) -> Effect Unit
_queryBoth' (Tuple qa ea) (Tuple qb eb) f = do
   ma <- _querySelector qa ea
   bs <- _querySelectorAll qb eb
   for_ ma \a -> 
     f a bs

_queryBoth'' :: forall a. Tuple String Element -> Tuple String Element -> (Array Node -> Array Node -> Effect a) -> Effect a
_queryBoth'' (Tuple qa ea) (Tuple qb eb) f = do
   as <- _querySelectorAll qa ea
   bs <- _querySelectorAll qb eb
   f as bs

_querySelector :: String -> Element -> Effect (Maybe Element)
_querySelector s n = querySelector (QuerySelector s) (toParentNode n)

_querySelectorAll :: String -> Element -> Effect (Array Node)
_querySelectorAll s n = toArray =<< querySelectorAll (QuerySelector s) (toParentNode n)

_fromNode :: Node -> Element
_fromNode e = unsafePartial $ fromJust (fromNode e)

_fromElement :: Element -> HTMLElement
_fromElement e = unsafePartial $ fromJust (HE.fromElement e)

_getElementById :: String -> HTMLDocument -> Effect (Maybe Element)
_getElementById s = getElementById s <<< toNonElementParentNode <<< toDocument

_doc :: Effect HTMLDocument
_doc = document =<< window 

_body :: Effect HTMLElement
_body = unsafePartial $ pure <<< fromJust =<< HD.body =<< _doc

_mt :: forall a. Effect (Maybe a) -> MaybeT Effect a
_mt = MaybeT

_mt_pure :: forall a. Maybe a -> MaybeT Effect a
_mt_pure = MaybeT <<< pure
