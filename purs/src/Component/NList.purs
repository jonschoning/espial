module Component.NList where

import Prelude

import Data.Array (drop, foldMap)
import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (null, split, take) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
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
import Util (class_, attr)

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
  render st = 
    HH.div_ (map (renderNote st) (st.notes))

  renderNote :: NLState -> Note -> H.ComponentHTML NLQuery
  renderNote st bm =
    div [ id_ (show bm.id) , class_ ("note w-100 mw7 pa1 mb2")] $
       [ div [ class_ "display" ] $
         [ a [ href (linkToFilterSingle bm.id), target "_blank", class_ ("link f5 lh-title")]
           [ text $ if S.null bm.title then "[no title]" else bm.title ]
         , br_
         , div [ class_ "description mt1 mid-gray" ] (toTextarea (S.take 200 bm.text))
         , a [ class_ "link f7 dib gray w4", title (maybe bm.created snd mmoment) , href (linkToFilterSingle bm.id) ]
           [ text (maybe " " fst mmoment) ]
         ]
       ]
    where
      mmoment = mmoment8601 bm.created

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

-- | Component State
type NState =
  { note :: Note
  , deleteAsk :: Boolean
  , edit :: Boolean
  }

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
    , deleteAsk: false
    , edit: false
    }

  render :: NState -> H.ComponentHTML NQuery
  render st = 
    renderNote st st.note

  renderNote :: NState -> Note -> H.ComponentHTML NQuery
  renderNote st bm =
    div [ id_ (show bm.id) , class_ ("note w-100 mw7 pa1 mb2")] $
       [ div [ class_ "display" ] $
         [ div [ class_ ("link f5 lh-title")]
           [ text $ if S.null bm.title then "[no title]" else bm.title ]
         , br_
         , div [ class_ "description mt1 mid-gray" ] (toTextarea bm.text)
         , div [ class_ "link f7 dib gray w4", title (maybe bm.created snd mmoment) ]
           [ text (maybe " " fst mmoment) ]
         ]
       ]
    where
      mmoment = mmoment8601 bm.created

  toTextarea input =
    S.split (Pattern "\n") input
    # foldMap (\x -> [br_, text x])
    # drop 1

  -- | Handle UI Events
  onEvent :: NQuery ~> H.ComponentDSL NState NQuery Void Aff
  onEvent (NNop next) = pure next
