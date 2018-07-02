module Globals where

import Data.Function.Uncurried

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (Unit, pure, ($))
import Web.DOM (Node)
import Web.HTML (HTMLElement)

type AppData =
    { csrfHeaderName :: String
    , csrfCookieName :: String
    , csrfToken :: String
    , homeR :: String
    , authRlogoutR :: String
    }

foreign import _app :: Fn0 AppData

app' :: Unit -> AppData
app' _ = runFn0 _app

foreign import _closest :: forall a. Fn4 (a -> Maybe a) (Maybe a) String Node (Maybe Node)

closest :: String -> Node -> Effect (Maybe Node)
closest selector node = pure $ runFn4 _closest Just Nothing selector node 

foreign import _replace_iso_timestamps :: Fn0 Unit

replaceIsoTimestamps :: Unit -> Effect Unit
replaceIsoTimestamps _ = pure $ runFn0 _replace_iso_timestamps

foreign import _innerHtml :: Fn2 HTMLElement String Unit

innerHtml :: HTMLElement -> String -> Effect Unit
innerHtml n c = pure $ runFn2 _innerHtml n c
