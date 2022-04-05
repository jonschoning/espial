module Component.NNote where

import Prelude hiding (div)

import Affjax (printError)
import Affjax.StatusCode (StatusCode(..))
import App (destroyNote, editNote)
import Component.Markdown as Markdown
import Data.Array (drop, foldMap)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, use, (%=), (.=))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.String (null, split) as S
import Data.String (null, stripPrefix)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Globals (app', mmoment8601, setFocus, closeWindow)
import Halogen as H
import Halogen.HTML (br_, button, div, form, input, label, p, span, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onChecked, onClick, onSubmit, onValueChange)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), autofocus, checked, for, id, name, rows, title, type_, value)
import Model (Note)
import Type.Proxy (Proxy(..))
import Util (_curQuerystring, _doc, _loc, _lookupQueryStringValue, class_, fromNullableStr, ifElseH, whenH)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.HTMLDocument (referrer)
import Web.HTML.Location (origin, setHref)

data NAction
  = NNop
  | NEditField EditField
  | NEditSubmit Event
  | NEdit Boolean
  | NDeleteAsk Boolean
  | NDestroy

type NState =
  { note :: Note
  , edit_note :: Note
  , deleteAsk :: Boolean
  , edit :: Boolean
  , destroyed :: Boolean
  , apiError :: Maybe String
  }

_note :: Lens' NState Note
_note = lens _.note (_ { note = _ })

_edit_note :: Lens' NState Note
_edit_note = lens _.edit_note (_ { edit_note = _ })

_edit :: Lens' NState Boolean
_edit = lens _.edit (_ { edit = _ })

_apiError :: Lens' NState (Maybe String)
_apiError = lens _.apiError (_ { apiError = _ })

-- | FormField Edits
data EditField
  = Etitle String
  | Etext String
  | EisMarkdown Boolean
  | Eshared Boolean

_markdown = Proxy :: Proxy "markdown"

type ChildSlots =
  ( markdown :: Markdown.Slot Unit
  )

nnote :: forall q i o. Note -> H.Component q i o Aff
nnote st' =
  H.mkComponent
    { initialState: const (mkState st')
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  app = app' unit

  mkState note' =
    { note: note'
    , edit_note: note'
    , deleteAsk: false
    , edit: note'.id <= 0
    , destroyed: false
    , apiError: Nothing
    }

  render :: NState -> H.ComponentHTML NAction ChildSlots Aff
  render st@{ note, edit_note, apiError } =
    ifElseH st.destroyed
       display_destroyed
       (const (ifElseH st.edit
                 renderNote_edit
                 renderNote))
    where

      renderNote _ =
        div [ id (show note.id) , class_ ("note w-100 mw7 pa1 mb2")] $
           [ div [ class_ "display" ] $
             [ div [ class_ ("link f5 lh-title")]
               [ text $ if S.null note.title then "[no title]" else note.title ]
             , br_
             , if note.isMarkdown
               then div [ class_ "description mt1" ] [ HH.slot _markdown unit Markdown.component note.text absurd ]
               else div [ class_ "description mt1 mid-gray" ] (toTextarea note.text)
             , div [ class_ "link f7 dib gray w4"]
                 [ span [title (maybe note.created snd (mmoment note))]
                   [text (maybe " " fst (mmoment note))]
                 , text " - "
                 , span [ class_ ("gray")]
                   [ text $ if note.shared then "public" else "private" ]
               ]
             ]
           , whenH app.dat.isowner $ \_ ->
               div [ class_ "edit_links db mt3" ]
                 [ button [ type_ ButtonButton, onClick \_ -> NEdit true, class_ "edit light-silver hover-blue" ] [ text "edit  " ]
                 , div [ class_ "delete_link di" ]
                   [ button [ type_ ButtonButton, onClick \_ -> NDeleteAsk true, class_ ("delete light-silver hover-blue" <> guard st.deleteAsk " dn") ] [ text "delete" ]
                   , span ([ class_ ("confirm red" <> guard (not st.deleteAsk) " dn") ] )
                     [ button [ type_ ButtonButton, onClick \_ -> NDeleteAsk false] [ text "cancel / " ]
                     , button [ type_ ButtonButton, onClick \_ -> NDestroy, class_ "red" ] [ text "destroy" ]
                     ]
                   ]
                 ]
               
           ]

      renderNote_edit _ =
        form [ onSubmit NEditSubmit ]
          [ whenH (isJust apiError)
              (alert_notification (fromMaybe "" apiError))
          , p [ class_ "mt2 mb1"] [ text "title:" ]
          , input [ type_ InputText , class_ "title w-100 mb1 pt1 edit_form_input" , name "title"
                  , value (edit_note.title) , onValueChange (editField Etitle), autofocus (null edit_note.title)
            ]
          , br_
          , p [ class_ "mt2 mb1"] [ text "description:" ]
          , textarea [ id (notetextid edit_note), class_ "description w-100 mb1 pt1 edit_form_input" , name "text", rows 25
                     , value (edit_note.text) , onValueChange (editField Etext)
            ]
          , div [ class_ "edit_form_checkboxes mb3"]
            [ input [ type_ InputCheckbox , class_ "is-markdown pointer" , id "edit_ismarkdown", name "ismarkdown"
                     , checked (edit_note.isMarkdown) , onChecked (editField EisMarkdown) ]
             , text " "
             , label [ for "edit_ismarkdown" , class_ "mr2" ] [ text "use markdown?" ]
             , br_
            ]
          , div [ class_ "edit_form_checkboxes mb3"]
            [ input [ type_ InputCheckbox , class_ "is-markdown pointer" , id "edit_shared", name "shared"
                    , checked (edit_note.shared) , onChecked (editField Eshared) ]
            , text " "
            , label [ for "edit_shared" , class_ "mr2" ] [ text "public?" ]
            , br_
            ]
          , input [ type_ InputSubmit
                  , class_ "mr1 pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim"
                  , value "save" ]
          , text " "
          , input [ type_ InputReset
                  , class_ "pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim"
                  , value "cancel"
                  , onClick \_ -> NEdit false
                  ]
          ]

      display_destroyed _ = p [ class_ "red"] [text "you killed this note"]

      alert_notification alert_text _ = 
        div [ class_ "alert alert-err" ] [ text alert_text ]

      mmoment n = mmoment8601 n.created
      editField :: forall a. (a -> EditField) -> a -> NAction
      editField f = NEditField <<< f
      toTextarea input =
        S.split (Pattern "\n") input
        # foldMap (\x -> [br_, text x])
        # drop 1

  notetextid note = show note.id <> "_text"

  handleAction :: NAction -> H.HalogenM NState NAction ChildSlots o Aff Unit
  handleAction (NNop) = pure unit

  -- | EditField
  handleAction (NEditField f) = do
    _edit_note %= case f of
      Etitle e -> _ { title = e }
      Etext e -> _ { text = e }
      EisMarkdown e -> _ { isMarkdown = e }
      Eshared e -> _ { shared = e }

  -- | Delete
  handleAction (NDeleteAsk e) = do
    H.modify_ (_ { deleteAsk = e })

  -- | Destroy
  handleAction (NDestroy) = do
    note <- use _note
    void $ H.liftAff (destroyNote note.id)
    H.modify_ (_ { destroyed = true })

  -- | Start/Stop Editing
  handleAction (NEdit e) = do
    note <- use _note
    _edit_note .= note
    _edit .= e
    qs <- liftEffect _curQuerystring
    case {e:e, q:_lookupQueryStringValue qs "next"} of
      {e:false, q:Just "closeWindow"} -> liftEffect $ closeWindow =<< window
      _ -> H.liftEffect $ whenM (pure e) (setFocus (notetextid note))
    

  -- | Submit
  handleAction (NEditSubmit e) = do
    H.liftEffect (preventDefault e)
    edit_note <- use _edit_note
    _apiError .= Nothing
    H.liftAff (editNote edit_note) >>= case _ of
      Left affErr -> do
        _apiError .= Just (printError affErr)
        liftEffect $ log (printError affErr)
      Right { status: StatusCode s } | s >= 200 && s < 300 -> do
        qs <- liftEffect _curQuerystring
        doc <- liftEffect $ _doc
        ref <- liftEffect $ referrer doc
        loc <- liftEffect $ _loc
        org <- liftEffect $ origin loc
        case _lookupQueryStringValue qs "next" of
          Just "closeWindow" -> liftEffect $ closeWindow =<< window
          Just "back" -> liftEffect $
            if isJust (stripPrefix (Pattern org) ref)
              then setHref ref loc
              else setHref org loc
          _ -> if (edit_note.id == 0)
                 then liftEffect $ setHref (fromNullableStr app.noteR) =<< _loc
                 else do
                   _note .= edit_note
                   _edit .= false
      Right res -> do
        _apiError .= Just (res.body)
        liftEffect $ log (res.body)
