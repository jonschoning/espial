module Component.AccountSettings where

import Prelude hiding (div)

import App (editAccountSettings)
import Data.Lens (Lens', lens, use, (%=))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Globals (app')
import Halogen as H
import Halogen.HTML (HTML, div, input, text)
import Halogen.HTML.Elements (label)
import Halogen.HTML.Events (onChecked)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..), checked, for, id_, name, type_)
import Model (AccountSettings)
import Util (class_)
import Web.Event.Event (Event)

type UState =
  { us :: AccountSettings
  }

_us :: Lens' UState AccountSettings
_us = lens _.us (_ { us = _ })

data UQuery a
  = UEditField EditField a
  | USubmit Event a

data EditField
  = EarchiveDefault Boolean
  | EprivateDefault Boolean
  | EprivacyLock Boolean


-- | The bookmark component definition.
usetting :: AccountSettings -> H.Component HTML UQuery Unit Unit Aff
usetting u' =
  H.component
    { initialState: const (mkState u')
    , render
    , eval
    , receiver: const Nothing
    }
  where
  app = app' unit

  mkState u =
    { us: u
    }

  render :: UState -> H.ComponentHTML UQuery
  render { us } =
    div [ class_ "settings-form" ]
    [ div [ class_ "fw7 mb2"] [ text "Account Settings" ]
    , div [ class_ "flex items-center mb2" ]
      [ input [ type_ InputCheckbox , class_ "pointer mr2" , id_ "archiveDefault", name "archiveDefault"
              , checked (us.archiveDefault) , onChecked (editField EarchiveDefault) ]
      , label [ for "archiveDefault", class_ "lh-copy" ]
        [ text "Archive Non-Private Bookmarks (archive.li)" ]
      ]
    , div [ class_ "flex items-center mb2" ]
      [ input [ type_ InputCheckbox , class_ "pointer mr2" , id_ "privateDefault", name "privateDefault"
              , checked (us.privateDefault) , onChecked (editField EprivateDefault) ]
      , label [ for "privateDefault", class_ "lh-copy"  ]
        [ text "Default new bookmarks to Private" ]
      ]
    , div [ class_ "flex items-center mb2" ]
      [ input [ type_ InputCheckbox , class_ "pointer mr2" , id_ "privacyLock", name "privacyLock"
              , checked (us.privacyLock) , onChecked (editField EprivacyLock) ]
      , label [ for "privacyLock", class_ "lh-copy"  ]
        [ text "Privacy Lock (Private Account)" ]
      ]
    ]
    where
      editField :: forall a. (a -> EditField) -> a -> Maybe (UQuery Unit)
      editField f = HE.input UEditField <<< f

  eval :: UQuery ~> H.ComponentDSL UState UQuery Unit Aff
  eval (UEditField f next) = do
    _us %= case f of
      EarchiveDefault e -> _ { archiveDefault = e }
      EprivateDefault e -> _ { privateDefault = e }
      EprivacyLock e -> _ { privacyLock = e }
    pure next

  eval (USubmit e next) = do
    us <- use _us
    void $ H.liftAff (editAccountSettings us)
    pure next
