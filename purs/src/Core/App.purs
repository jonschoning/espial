module App where

import Prelude

import Affjax.RequestBody as AXReq
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as AXRes
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web (Response, Error)
import Affjax.Web (defaultRequest) as AX
import Affjax.Web as Ax
import Data.Array ((:))
import Data.Either (Either(..), hush)
import Data.FormURLEncoded (FormURLEncoded)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded, applicationJSON)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Globals (app')
import Model (Bookmark, Bookmark'(..), Note, Note'(..), AccountSettings, AccountSettings'(..), TagCloudMode, TagCloudMode'(..), TagCloud)
import Simple.JSON as J
import Unsafe.Coerce (unsafeCoerce)
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

destroy :: Int -> Aff (Either Error (Response Unit))
destroy bid =
  fetchUrlEnc DELETE ("bm/" <> show bid) Nothing AXRes.ignore

markRead :: Int -> Aff (Either Error (Response Unit))
markRead bid = do
  let path = "bm/" <> show bid <> "/read"
  fetchUrlEnc POST path Nothing AXRes.ignore

editBookmark :: Bookmark -> Aff (Either Error (Response String))
editBookmark bm =  do
    fetchJson POST "api/add" (Just (Bookmark' bm)) AXRes.string

editNote :: Note -> Aff (Either Error (Response String))
editNote bm =  do
    fetchJson POST "api/note/add" (Just (Note' bm)) AXRes.string

lookupTitle :: Bookmark -> Aff (Maybe String)
lookupTitle bm = do
  eres <- fetchJson POST "api/lookuptitle" (Just (Bookmark' bm)) AXRes.string
  pure $ hush eres >>= \res ->
    if (res.status == StatusCode 200)
    then Just res.body
    else Nothing
      
getTagCloud :: TagCloudMode -> Aff (Maybe TagCloud)
getTagCloud mode = do
    eres <- fetchJson POST "api/tagcloud" (Just (TagCloudMode' mode)) AXRes.json
    pure $ hush eres >>= \res ->
        pure (unsafeCoerce res.body)

updateTagCloudMode :: TagCloudMode -> Aff (Either Error (Response Unit))
updateTagCloudMode mode = do
    fetchJson POST "api/tagcloudmode" (Just (TagCloudMode' mode)) AXRes.ignore
  
destroyNote :: Int -> Aff (Either Error (Response Unit))
destroyNote nid =  do
  fetchUrlEnc DELETE ("api/note/" <> show nid) Nothing AXRes.ignore

editAccountSettings :: AccountSettings -> Aff (Either Error (Response Unit))
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
  -> Aff (Either Error (Response a))
fetchJson method path content rt =
    fetchPath method path [ContentType applicationJSON] (AXReq.string <<< J.writeJSON <$> content) rt

fetchUrlEnc
  :: forall a.
     Method
  -> String
  -> Maybe FormURLEncoded
  -> AXRes.ResponseFormat a
  -> Aff (Either Error (Response a))
fetchUrlEnc method path content rt =
    fetchPath method path [ContentType applicationFormURLEncoded] (AXReq.FormURLEncoded <$> content) rt

fetchPath
  :: forall a.
     Method
  -> String
  -> Array RequestHeader
  -> Maybe AXReq.RequestBody
  -> AXRes.ResponseFormat a
  -> Aff (Either Error (Response a))
fetchPath method path headers content rt =
    fetchUrl method ((app' unit).homeR <> path) headers content rt

fetchUrl
  :: forall a.
     Method
  -> String
  -> Array RequestHeader
  -> Maybe AXReq.RequestBody
  -> AXRes.ResponseFormat a
  -> Aff (Either Error (Response a))
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
