module Component.NList where

import Data.Argonaut
import Prelude

import App (destroyNote, editNote)
import Control.Monad.State.Class (class MonadState)
import Data.Array (drop, foldMap)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String (null, split, take) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Globals (app', mmoment8601)
import Halogen as H
import Halogen.HTML (HTML, a, br_, button, div, div_, form, input, label, p, p_, span, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onSubmit, onValueChange, onChecked, onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..), autocomplete, checked, for, href, id_, name, required, rows, target, title, type_, value)
import Model (Note, NoteId)
import Util (_curQuerystring, _loc, attr, class_)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location (setHref)

data NLQuery a
  = NLNop a

type NLSlot = NoteId

-- | Component State
type NLState =
  { notes :: Array Note
  , cur :: Maybe NLSlot
  , deleteAsk:: Boolean
  , edit :: Boolean
  }


nlist :: Array Note -> H.Component HH.HTML NLQuery Unit Void Aff
nlist st' =
  H.component
    { initialState: const (mkState st')
    , render
    , eval: onEvent
    , receiver: const Nothing
    }
  where
  app = app' unit

  -- | Initialize Component State
  mkState notes' =
    { notes: notes'
    , cur: Nothing
    , deleteAsk: false
    , edit: false
    }

  render :: NLState -> H.ComponentHTML NLQuery
  render st@{ notes } =
    HH.div_ (map renderNote notes)
    where
      renderNote :: Note -> H.ComponentHTML NLQuery
      renderNote bm =
        div [ id_ (show bm.id) , class_ ("note w-100 mw7 pa1 mb2")] $
           [ div [ class_ "display" ] $
             [ a [ href (linkToFilterSingle bm.id), target "_blank", class_ ("link f5 lh-title")]
               [ text $ if S.null bm.title then "[no title]" else bm.title ]
             , br_
             , div [ class_ "description mt1 mid-gray" ] (toTextarea (S.take 200 bm.text))
             , a [ class_ "link f7 dib gray w4", title (maybe bm.created snd (mmoment bm)) , href (linkToFilterSingle bm.id) ]
               [ text (maybe " " fst (mmoment bm)) ]
             ]
           ]

  mmoment bm = mmoment8601 bm.created
  linkToFilterSingle nid = app.userR <> "/notes/" <> show nid
  toTextarea input =
    S.split (Pattern "\n") input
    # foldMap (\x -> [br_, text x])
    # drop 1

  -- | Handle UI Events
  onEvent :: NLQuery ~> H.ComponentDSL NLState NLQuery Void Aff
  onEvent (NLNop next) = pure next


data NQuery a
  = NNop a
  | NEditField EditField a
  | NEditSubmit Event a
  | NEdit Boolean a
  | NDeleteAsk Boolean a
  | NDestroy a

-- | Component State
type NState =
  { note :: Note
  , edit_note :: Note
  , deleteAsk :: Boolean
  , edit :: Boolean
  , destroyed :: Boolean
  }

-- | FormField Edits
data EditField
  = Etitle String
  | Etext String

nnote :: Note -> H.Component HH.HTML NQuery Unit Void Aff
nnote st' =
  H.component
    { initialState: const (mkState st')
    , render
    , eval: onEvent
    , receiver: const Nothing
    }
  where
  app = app' unit

  -- | Initialize Component State
  mkState note' =
    { note: note'
    , edit_note: note'
    , deleteAsk: false
    , edit: note'.id <= 0
    , destroyed: false
    }

  render :: NState -> H.ComponentHTML NQuery
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
             , div [ class_ "description mt1 mid-gray" ] (toTextarea note.text)
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
          [ p [ class_ "mt2 mb1"] [ text "Title:" ]
          , input [ type_ InputText , class_ "title w-100 mt2 mb1 pt1 f7 edit_form_input" , name "title"
            , value (edit_note.title) , onValueChange (editField Etitle)
            ]
          , br_
          , p [ class_ "mt2 mb1"] [ text "Description:" ]
          , textarea [ class_ "description w-100 mt2 mb1 pt1 f7 edit_form_input" , name "text", rows 30
            , value (edit_note.text) , onValueChange (editField Etext)
            ]
          , div [ class_ "edit_form_checkboxes mv3"]
              [ input [ type_ InputSubmit , class_ "mr1 pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "save" ]
              , text " "
              , input [ type_ InputReset , class_ "pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "cancel"
                , onClick (HE.input_ (NEdit false))
                ]
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


  -- | Handle UI Events
  onEvent :: NQuery ~> H.ComponentDSL NState NQuery Void Aff
  onEvent (NNop next) = pure next

  -- | EditField
  onEvent (NEditField f next) = do
    modifyEdit $ case f of
      Etitle e -> _ { title = e }
      Etext e -> _ { text = e }
    pure next
    where
      modifyEdit :: forall m. MonadState NState m => (Note -> Note) -> m Unit
      modifyEdit g = H.modify_ \s -> s { edit_note = g s.edit_note  }

  -- | Delete
  onEvent (NDeleteAsk e next) = do
    H.modify_ (_ { deleteAsk = e })
    pure next

  -- | Destroy
  onEvent (NDestroy next) = do
    state <- H.get
    void $ H.liftAff (destroyNote state.note.id)
    H.put $ state { destroyed = true }
    pure next

  -- | Start/Stop Editing
  onEvent (NEdit e next) = do
    state <- H.get
    let newState = state { edit = e, edit_note = state.note }
    H.put newState
    pure next

  -- | Submit
  onEvent (NEditSubmit e next) = do
    H.liftEffect (preventDefault e)
    state <- H.get
    let nt = state.edit_note
    res <- H.liftAff (editNote nt)
    let nt' = fromMaybe nt (toNumber res.response >>= I.fromNumber >>= \n -> Just (nt { id = n }))
    H.put $ state { edit = false, note = nt', edit_note = nt' }
    pure next
