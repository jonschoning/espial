module Component.NNote where

import Prelude hiding (div)

import App (destroyNote, editNote)
import Component.Markdown as Markdown
import Data.Array (drop, foldMap)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, use, (%=), (.=))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String (null, split) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Globals (app', mmoment8601)
import Halogen as H
import Halogen.HTML (br_, button, div, form, input, label, p, span, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onChecked, onClick, onSubmit, onValueChange)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..), checked, for, id_, name, rows, title, type_, value)
import Model (Note)
import Util (_loc, class_, fromNullableStr)
import Web.Event.Event (Event, preventDefault)
import Web.HTML.Location (setHref)

data NQuery a
  = NNop a
  | NEditField EditField a
  | NEditSubmit Event a
  | NEdit Boolean a
  | NDeleteAsk Boolean a
  | NDestroy a

type NState =
  { note :: Note
  , edit_note :: Note
  , deleteAsk :: Boolean
  , edit :: Boolean
  , destroyed :: Boolean
  }

_note :: Lens' NState Note
_note = lens _.note (_ { note = _ })

_edit_note :: Lens' NState Note
_edit_note = lens _.edit_note (_ { edit_note = _ })

_edit :: Lens' NState Boolean
_edit = lens _.edit (_ { edit = _ })

-- | FormField Edits
data EditField
  = Etitle String
  | Etext String
  | EisMarkdown Boolean

type NChildQuery = Markdown.MQuery

nnote :: Note -> H.Component HH.HTML NQuery Unit Void Aff
nnote st' =
  H.parentComponent
    { initialState: const (mkState st')
    , render
    , eval
    , receiver: const Nothing
    }
  where
  app = app' unit

  mkState note' =
    { note: note'
    , edit_note: note'
    , deleteAsk: false
    , edit: note'.id <= 0
    , destroyed: false
    }

  render :: NState -> H.ParentHTML NQuery NChildQuery Unit Aff
  render st@{ note, edit_note } =
    if st.destroyed
       then display_destroyed
       else
        if st.edit
          then renderNote_edit
          else renderNote
    where

      renderNote =
        div [ id_ (show note.id) , class_ ("note w-100 mw7 pa1 mb2")] $
           [ div [ class_ "display" ] $
             [ div [ class_ ("link f5 lh-title")]
               [ text $ if S.null note.title then "[no title]" else note.title ]
             , br_
             , if note.isMarkdown
               then div [ class_ "description mt1" ] [ HH.slot unit Markdown.component note.text absurd ]
               else div [ class_ "description mt1 mid-gray" ] (toTextarea note.text)
             , div [ class_ "link f7 dib gray w4", title (maybe note.created snd (mmoment note)) ]
               [ text (maybe " " fst (mmoment note)) ]
             ]
           ]
           <> -- | Render Action Links
           [ div [ class_ "edit_links db mt3" ]
             [ button [ type_ ButtonButton, onClick (HE.input_ (NEdit true)), class_ "edit light-silver hover-blue" ] [ text "edit  " ]
             , div [ class_ "delete_link di" ]
               [ button [ type_ ButtonButton, onClick (HE.input_ (NDeleteAsk true)), class_ ("delete light-silver hover-blue" <> guard st.deleteAsk " dn") ] [ text "delete" ]
               , span ([ class_ ("confirm red" <> guard (not st.deleteAsk) " dn") ] )
                 [ button [ type_ ButtonButton, onClick (HE.input_ (NDeleteAsk false))] [ text "cancel / " ]
                 , button [ type_ ButtonButton, onClick (HE.input_ NDestroy), class_ "red" ] [ text "destroy" ]
                 ] 
               ]
             ]
           ]

      renderNote_edit =
        form [ onSubmit (HE.input NEditSubmit) ]
          [ p [ class_ "mt2 mb1"] [ text "title:" ]
          , input [ type_ InputText , class_ "title w-100 mb1 pt1 f7 edit_form_input" , name "title"
                  , value (edit_note.title) , onValueChange (editField Etitle)
            ]
          , br_
          , p [ class_ "mt2 mb1"] [ text "description:" ]
          , textarea [ class_ "description w-100 mb1 pt1 f7 edit_form_input" , name "text", rows 30
                     , value (edit_note.text) , onValueChange (editField Etext)
            ]
          , div [ class_ "edit_form_checkboxes mb3"]
            [ input [ type_ InputCheckbox , class_ "is-markdown pointer" , id_ "edit_ismarkdown", name "ismarkdown"
                     , checked (edit_note.isMarkdown) , onChecked (editField EisMarkdown) ]
             , text " "
             , label [ for "edit_ismarkdown" , class_ "mr2" ] [ text "use markdown?" ]
             , br_
            ]
          , input [ type_ InputSubmit , class_ "mr1 pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "save" ]
          , text " "
          , input [ type_ InputReset , class_ "pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "cancel"
                  , onClick (HE.input_ (NEdit false))
                  ]
          ]

      display_destroyed = p [ class_ "red"] [text "you killed this note"]

      mmoment n = mmoment8601 n.created
      editField :: forall a. (a -> EditField) -> a -> Maybe (NQuery Unit)
      editField f = HE.input NEditField <<< f
      toTextarea input =
        S.split (Pattern "\n") input
        # foldMap (\x -> [br_, text x])
        # drop 1


  eval :: NQuery ~> H.ParentDSL NState NQuery NChildQuery Unit Void Aff
  eval (NNop next) = pure next

  -- | EditField
  eval (NEditField f next) = do
    _edit_note %= case f of
      Etitle e -> _ { title = e }
      Etext e -> _ { text = e }
      EisMarkdown e -> _ { isMarkdown = e }
    pure next

  -- | Delete
  eval (NDeleteAsk e next) = do
    H.modify_ (_ { deleteAsk = e })
    pure next

  -- | Destroy
  eval (NDestroy next) = do
    note <- use _note
    void $ H.liftAff (destroyNote note.id)
    H.modify_ (_ { destroyed = true })
    pure next

  -- | Start/Stop Editing
  eval (NEdit e next) = do
    note <- use _note
    _edit_note .= note
    _edit .= e
    pure next

  -- | Submit
  eval (NEditSubmit e next) = do
    H.liftEffect (preventDefault e)
    edit_note <- use _edit_note
    res <- H.liftAff (editNote edit_note)
    case res.body of
      Left err -> pure next
      Right r -> do
        if (edit_note.id == 0)
          then do
            liftEffect (setHref (fromNullableStr app.noteR) =<< _loc)
          else do
            _note .= edit_note
            _edit .= false
        pure next
