module App where

import Prelude
import Globals (app')

import Data.Array ((:))
import Data.Either (Either(..))
import Data.FormURLEncoded (FormURLEncoded)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationFormURLEncoded)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Network.HTTP.Affjax (affjax, AffjaxResponse)
import Network.HTTP.Affjax (defaultRequest) as AX
import Network.HTTP.Affjax.Request as AXReq
import Network.HTTP.Affjax.Response as AXRes
import Network.HTTP.RequestHeader (RequestHeader(..))
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)

-- import Data.Argonaut.Core as J

logoutE :: Event -> Effect Unit
logoutE e = void <<< launchAff <<< logout =<< preventDefault e

logout :: Unit -> Aff Unit
logout u = do
  void $ fetchUrl POST app.authRlogoutR [] Nothing AXRes.ignore
  liftEffect $ window >>= location >>= reload
  where
    app = app' u

fetchUrlEnc
  :: forall a.
     Method
  -> String
  -> Maybe FormURLEncoded
  -> AXRes.Response a
  -> Aff (AffjaxResponse a)
fetchUrlEnc method path content rt =
    fetchPath method path [ContentType applicationFormURLEncoded] (AXReq.FormURLEncoded <$> content) rt

fetchPath
  :: forall a.
     Method
  -> String
  -> Array RequestHeader
  -> Maybe AXReq.Request
  -> AXRes.Response a
  -> Aff (AffjaxResponse a)
fetchPath method path headers content rt =
    fetchUrl method ((app' unit).homeR <> path) headers content rt

fetchUrl
  :: forall a.
     Method
  -> String
  -> Array RequestHeader
  -> Maybe AXReq.Request
  -> AXRes.Response a
  -> Aff (AffjaxResponse a)
fetchUrl method url headers content rt =
  affjax rt
    AX.defaultRequest
    { url = url
    , method = Left method
    , headers = RequestHeader app.csrfHeaderName app.csrfToken : headers
    , content = content
    }
  where
    app = app' unit
