module Util where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array (filter, find, mapMaybe)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), Replacement(..), drop, replaceAll, split, take)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import JSURI (decodeURIComponent, encodeURIComponent)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Element, Node)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.Element (fromNode, toParentNode)
import Web.DOM.NodeList (toArray)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (HTMLDocument, Location, window)
import Web.HTML.HTMLDocument (body) as HD
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement (fromElement) as HE
import Web.HTML.Location (search)
import Web.HTML.Window (document, location)

unsafeDecode :: String -> String
unsafeDecode str = unsafePartial $ fromJust $ decodeURIComponent str
  
-- Halogen

class_ :: forall r i. String -> HP.IProp ( "class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

attr :: forall r i. String -> String -> HP.IProp r i
attr a = HP.attr (HH.AttrName a)

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

_loc :: Effect Location
_loc = location =<< window 

type QueryStringArray = Array (Tuple String (Maybe String))

_curQuerystring :: Effect QueryStringArray
_curQuerystring = do
  loc <- _loc
  srh <- search loc
  pure $ _parseQueryString srh

_parseQueryString :: String -> QueryStringArray
_parseQueryString srh = do
  let qs = let srh' = take 1 srh in if (srh' == "#" || srh' == "?") then drop 1 srh else srh
  mapMaybe go $ (filter (_ /= "") <<< split (Pattern "&")) qs
  where
    decode = unsafeDecode <<< replaceAll (Pattern "+") (Replacement " ")
    go kv =
      case split (Pattern "=") kv of
        [k] -> Just (Tuple (decode k) Nothing)
        [k, v] -> Just (Tuple (decode k) (Just (decode v)))
        _ -> Nothing

_lookupQueryStringValue :: QueryStringArray -> String -> Maybe String
_lookupQueryStringValue qs k = do
  join $ map snd $ find ((_ == k) <<< fst) qs

_body :: Effect HTMLElement
_body = unsafePartial $ pure <<< fromJust =<< HD.body =<< _doc

_mt :: forall a. Effect (Maybe a) -> MaybeT Effect a
_mt = MaybeT

_mt_pure :: forall a. Maybe a -> MaybeT Effect a
_mt_pure = MaybeT <<< pure

encodeTag :: String -> String
encodeTag = fromMaybe "" <<< encodeURIComponent <<< replaceAll (Pattern "+") (Replacement "%2B")

dummyAttr :: forall r i. HP.IProp r i
dummyAttr = HP.attr (HH.AttrName "data-dummy") ""

whenP :: forall r i. Boolean -> HP.IProp r i -> HP.IProp r i
whenP b p = if b then p else dummyAttr

maybeP :: forall a r i. Maybe a -> (a -> HP.IProp r i) -> HP.IProp r i
maybeP m p = maybe dummyAttr p m
  
whenC :: Boolean -> ClassName -> ClassName
whenC b c = if b then c else ClassName ""

whenH :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenH b k = if b then k unit else HH.text ""

whenA :: forall t. Boolean -> (Unit -> Array t) -> Array t
whenA b k = if b then k unit else []

ifElseH :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> (Unit -> HH.HTML p i) -> HH.HTML p i
ifElseH b f k = if b then f unit else k unit

ifElseA :: forall t. Boolean -> (Unit -> Array t) -> (Unit -> Array t) -> Array t
ifElseA b f k = if b then f unit else k unit

maybeH :: forall a p i. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeH m k = maybe (HH.text "") k m

fromNullableStr :: Nullable String -> String
fromNullableStr = fromMaybe "" <<< toMaybe

monthNames :: Array String
monthNames = ["january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december"]
