module Globals where

import Data.Function.Uncurried

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Model (Bookmark)
import Prelude (Unit, pure, ($))
import Web.DOM (Element, Node)
import Web.HTML (HTMLElement, HTMLFormElement, Window)
import Web.XHR.FormData (FormData)
import Data.Newtype (class Newtype)

type App =
    { csrfHeaderName :: String
    , csrfCookieName :: String
    , csrfParamName :: String
    , csrfToken :: String
    , homeR :: String
    , authRlogoutR :: String
    , userR :: Nullable String
    , noteR :: Nullable String
    , dat :: AppData
    }

type AppData =
  { bmarks :: Array Bookmark
  , bmark :: Bookmark
  , isowner :: Boolean
  }

foreign import _app :: Fn0 App

app' :: Unit -> App
app' _ = runFn0 _app

foreign import _closest :: forall a. Fn4 (a -> Maybe a) (Maybe a) String Node (Maybe Node)

closest :: String -> Node -> Effect (Maybe Node)
closest selector node = pure $ runFn4 _closest Just Nothing selector node 

foreign import _moment8601 :: Fn2 (String -> String -> Tuple String String) String (Tuple String String)

moment8601 :: String -> Effect (Tuple String String)
moment8601 s = pure $ runFn2 _moment8601 Tuple s

foreign import _mmoment8601 :: forall a. Fn4 (a -> Maybe a) (Maybe a) (String -> String -> Tuple String String) String (Maybe (Tuple String String))

mmoment8601 :: String -> Maybe (Tuple String String)
mmoment8601 s = runFn4 _mmoment8601 Just Nothing Tuple s

foreign import _innerHtml :: Fn1 HTMLElement String

innerHtml :: HTMLElement -> Effect String
innerHtml n = pure $ runFn1 _innerHtml n

foreign import _setInnerHtml :: Fn2 String HTMLElement HTMLElement

setInnerHtml :: String -> HTMLElement -> Effect HTMLElement
setInnerHtml c n = pure $ runFn2 _setInnerHtml c n

foreign import _createFormData :: Fn1 HTMLFormElement FormData

createFormData :: HTMLFormElement -> FormData
createFormData f = runFn1 _createFormData f

foreign import _createFormString :: Fn1 HTMLFormElement String

createFormString :: HTMLFormElement -> String
createFormString f = runFn1 _createFormString f


foreign import _createFormArray :: Fn1 HTMLFormElement (Array (Array String))

createFormArray :: HTMLFormElement -> (Array (Array String))
createFormArray f = runFn1 _createFormArray f

foreign import _getDataAttribute :: Fn2 String Element (Nullable String)

getDataAttribute :: String -> Element -> Effect (Maybe String)
getDataAttribute k n = pure $ toMaybe $ runFn2 _getDataAttribute k n

foreign import _setDataAttribute :: Fn3 String String Element Unit

setDataAttribute :: String -> String -> Element -> Effect Unit
setDataAttribute k v n = pure $ runFn3 _setDataAttribute k v n

foreign import _closeWindow :: Fn1 Window Unit

closeWindow :: Window -> Effect Unit
closeWindow win = pure $ runFn1 _closeWindow win

newtype RawHTML = RawHTML String

derive instance newtypeRawHTML :: Newtype RawHTML _
