module Logout where

import Prelude

import App (logout)
import Effect (Effect)
import Effect.Aff (launchAff)
import Web.Event.Event (Event, preventDefault)

logoutE :: Event -> Effect Unit
logoutE e = void <<< launchAff <<< logout =<< preventDefault e