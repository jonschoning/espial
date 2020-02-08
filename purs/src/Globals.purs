module Globals where

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Data.Function.Uncurried
import Effect.Uncurried
import Model (Bookmark)
import Prelude
import Web.DOM (Node)
import Web.HTML (HTMLElement, HTMLFormElement, Window)
import Web.XHR.FormData (FormData)

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

foreign import _closest :: forall a. EffectFn4 (a -> Maybe a) (Maybe a) String Node (Maybe Node)

closest :: String -> Node -> Effect (Maybe Node)
closest selector node = runEffectFn4 _closest Just Nothing selector node 

foreign import _moment8601 :: EffectFn2 (String -> String -> Tuple String String) String (Tuple String String)

moment8601 :: String -> Effect (Tuple String String)
moment8601 s = runEffectFn2 _moment8601 Tuple s

foreign import _mmoment8601 :: forall a. Fn4 (a -> Maybe a) (Maybe a) (String -> String -> Tuple String String) String (Maybe (Tuple String String))

mmoment8601 :: String -> Maybe (Tuple String String)
mmoment8601 s = runFn4 _mmoment8601 Just Nothing Tuple s

foreign import _innerHtml :: EffectFn1 HTMLElement String

innerHtml :: HTMLElement -> Effect String
innerHtml n = runEffectFn1 _innerHtml n

foreign import _setInnerHtml :: EffectFn2 String HTMLElement HTMLElement

setInnerHtml :: String -> HTMLElement -> Effect HTMLElement
setInnerHtml c n = runEffectFn2 _setInnerHtml c n

foreign import _createFormData :: Fn1 HTMLFormElement FormData

createFormData :: HTMLFormElement -> FormData
createFormData f = runFn1 _createFormData f

foreign import _createFormString :: Fn1 HTMLFormElement String

createFormString :: HTMLFormElement -> String
createFormString f = runFn1 _createFormString f


foreign import _createFormArray :: Fn1 HTMLFormElement (Array (Array String))

createFormArray :: HTMLFormElement -> (Array (Array String))
createFormArray f = runFn1 _createFormArray f

foreign import _closeWindow :: EffectFn1 Window Unit

closeWindow :: Window -> Effect Unit
closeWindow win = runEffectFn1 _closeWindow win

newtype RawHTML = RawHTML String

derive instance newtypeRawHTML :: Newtype RawHTML _

foreign import _setFocus :: EffectFn1 String Unit

setFocus :: String -> Effect Unit
setFocus s = runEffectFn1 _setFocus s

foreign import _toLocaleDateString :: Fn1 String String

toLocaleDateString :: String -> String
toLocaleDateString s = runFn1 _toLocaleDateString s
