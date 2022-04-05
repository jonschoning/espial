module Component.AccountSettings where

import Prelude hiding (div)

import App (editAccountSettings)
import Data.Lens (Lens', lens, use, (%=))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML (div, input, text)
import Halogen.HTML.Elements (label)
import Halogen.HTML.Events (onChecked)
import Halogen.HTML.Properties (InputType(..), checked, for, id, name, type_)
import Model (AccountSettings)
import Util (class_)
import Web.Event.Event (Event)

type UState =
  { us :: AccountSettings
  }

_us :: Lens' UState AccountSettings
_us = lens _.us (_ { us = _ })

data UAction
  = UEditField EditField
  | USubmit Event

data EditField
  = EarchiveDefault Boolean
  | EprivateDefault Boolean
  | EprivacyLock Boolean


-- | The bookmark component definition.
usetting :: forall q i o. AccountSettings -> H.Component q i o Aff
usetting u' =
  H.mkComponent
    { initialState: const (mkState u')
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  mkState u =
    { us: u
    }

  render :: forall m. UState -> H.ComponentHTML UAction () m
  render { us } =
    div [ class_ "settings-form" ]
    [ div [ class_ "fw7 mb2"] [ text "Account Settings" ]
    , div [ class_ "flex items-center mb2" ]
      [ input [ type_ InputCheckbox , class_ "pointer mr2" , id "archiveDefault", name "archiveDefault"
              , checked (us.archiveDefault) , onChecked (editField EarchiveDefault) ]
      , label [ for "archiveDefault", class_ "lh-copy" ]
        [ text "Archive Non-Private Bookmarks (archive.li)" ]
      ]
    , div [ class_ "flex items-center mb2" ]
      [ input [ type_ InputCheckbox , class_ "pointer mr2" , id "privateDefault", name "privateDefault"
              , checked (us.privateDefault) , onChecked (editField EprivateDefault) ]
      , label [ for "privateDefault", class_ "lh-copy"  ]
        [ text "Default new bookmarks to Private" ]
      ]
    , div [ class_ "flex items-center mb2" ]
      [ input [ type_ InputCheckbox , class_ "pointer mr2" , id "privacyLock", name "privacyLock"
              , checked (us.privacyLock) , onChecked (editField EprivacyLock) ]
      , label [ for "privacyLock", class_ "lh-copy"  ]
        [ text "Privacy Lock (Private Account)" ]
      ]
    ]
    where
      editField :: forall a. (a -> EditField) -> a -> UAction
      editField f = UEditField <<< f

  handleAction :: UAction -> H.HalogenM UState UAction () o Aff Unit
  handleAction (UEditField f) = do
    _us %= case f of
      EarchiveDefault e -> _ { archiveDefault = e }
      EprivateDefault e -> _ { privateDefault = e }
      EprivacyLock e -> _ { privacyLock = e }
    us <- use _us
    void $ H.liftAff (editAccountSettings us)

  handleAction (USubmit _) = do
    us <- use _us
    void $ H.liftAff (editAccountSettings us)
