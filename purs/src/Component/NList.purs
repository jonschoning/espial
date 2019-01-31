module Component.NList where

import Prelude hiding (div)

import Data.Array (drop, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.String (null, split, take) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Globals (app', mmoment8601)
import Halogen as H
import Halogen.HTML (a, br_, div, text)
import Halogen.HTML as HH
import Halogen.HTML.Properties (href, id_, title)
import Model (Note, NoteSlug)
import Util (class_, fromNullableStr)

data NLQuery a
  = NLNop a

type NLSlot = NoteSlug

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
    , eval
    , receiver: const Nothing
    }
  where
  app = app' unit

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
             [ a [ href (linkToFilterSingle bm.slug), class_ ("link f5 lh-title")]
               [ text $ if S.null bm.title then "[no title]" else bm.title ]
             , br_
             , div [ class_ "description mt1 mid-gray" ] (toTextarea (S.take 200 bm.text))
             , a [ class_ "link f7 dib gray w4", title (maybe bm.created snd (mmoment bm)) , href (linkToFilterSingle bm.slug) ]
               [ text (maybe " " fst (mmoment bm)) ]
             ]
           ]

  mmoment bm = mmoment8601 bm.created
  linkToFilterSingle slug = fromNullableStr app.userR <> "/notes/" <> slug
  toTextarea input =
    S.split (Pattern "\n") input
    # foldMap (\x -> [br_, text x])
    # drop 1

  eval :: NLQuery ~> H.ComponentDSL NLState NLQuery Void Aff
  eval (NLNop next) = pure next
