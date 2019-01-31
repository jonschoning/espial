module App where

import Prelude

import Affjax (Response, ResponseFormatError)
import Affjax (defaultRequest) as AX
import Affjax as Ax
import Affjax.RequestBody as AXReq
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRes
import Data.Argonaut (Json)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Globals (app')
import Model (Bookmark, Bookmark'(..), Note, Note'(..), AccountSettings, AccountSettings'(..))
import Simple.JSON as J
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

data StarAction = Star | UnStar
instance showStar :: Show StarAction where
  show Star = "star"
  show UnStar = "unstar"

toggleStar :: Int -> StarAction -> Aff Unit
toggleStar bid action = do
  let path = "bm/" <> show bid <> "/" <> show action
  void (fetchUrlEnc POST path Nothing AXRes.ignore)

destroy :: Int -> Aff (Response (Either ResponseFormatError Unit))
destroy bid =
  fetchUrlEnc DELETE ("bm/" <> show bid) Nothing AXRes.ignore

markRead :: Int -> Aff (Response (Either ResponseFormatError Unit))
markRead bid = do
  let path = "bm/" <> show bid <> "/read"
  fetchUrlEnc POST path Nothing AXRes.ignore

editBookmark :: Bookmark -> Aff (Response (Either ResponseFormatError Unit))
editBookmark bm =  do
    fetchJson POST "api/add" (Just (Bookmark' bm)) AXRes.ignore

editNote :: Note -> Aff (Response (Either ResponseFormatError Json))
editNote bm =  do
    fetchJson POST "api/note/add" (Just (Note' bm)) AXRes.json

destroyNote :: Int -> Aff (Response (Either ResponseFormatError Unit))
destroyNote nid =  do
  fetchUrlEnc DELETE ("api/note/" <> show nid) Nothing AXRes.ignore

editAccountSettings :: AccountSettings -> Aff (Response (Either ResponseFormatError Unit))
editAccountSettings us =  do
    fetchJson POST "api/accountSettings" (Just (AccountSettings' us)) AXRes.ignore

logout :: Unit -> Aff Unit
logout u = do
  void (fetchUrl POST app.authRlogoutR [] Nothing AXRes.ignore)
  liftEffect (window >>= location >>= reload)
  where
    app = app' u

fetchJson
  :: forall a b.
     J.WriteForeign b
  => Method
  -> String
  -> Maybe b
  -> AXRes.ResponseFormat a
  -> Aff (Response (Either ResponseFormatError a))
fetchJson method path content rt =
    fetchPath method path [ContentType applicationJSON] (AXReq.string <<< J.writeJSON <$> content) rt

fetchUrlEnc
  :: forall a.
     Method
  -> String
  -> Maybe FormURLEncoded
  -> AXRes.ResponseFormat a
  -> Aff (Response (Either ResponseFormatError a))
fetchUrlEnc method path content rt =
    fetchPath method path [ContentType applicationFormURLEncoded] (AXReq.FormURLEncoded <$> content) rt

fetchPath
  :: forall a.
     Method
  -> String
  -> Array RequestHeader
  -> Maybe AXReq.RequestBody
  -> AXRes.ResponseFormat a
  -> Aff (Response (Either ResponseFormatError a))
fetchPath method path headers content rt =
    fetchUrl method ((app' unit).homeR <> path) headers content rt

fetchUrl
  :: forall a.
     Method
  -> String
  -> Array RequestHeader
  -> Maybe AXReq.RequestBody
  -> AXRes.ResponseFormat a
  -> Aff (Response (Either ResponseFormatError a))
fetchUrl method url headers content rt =
  Ax.request
    AX.defaultRequest
    { url = url
    , method = Left method
    , headers = RequestHeader app.csrfHeaderName app.csrfToken : headers
    , content = content
    , responseFormat = rt
    }
  where
    app = app' unit
