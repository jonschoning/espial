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

data NLAction
  = NLNop

type NLState =
  { notes :: Array Note
  , cur :: Maybe NoteSlug
  , deleteAsk:: Boolean
  , edit :: Boolean
  }


nlist :: forall q i o. Array Note -> H.Component HH.HTML q i o Aff
nlist st' =
  H.mkComponent
    { initialState: const (mkState st')
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  app = app' unit

  mkState notes' =
    { notes: notes'
    , cur: Nothing
    , deleteAsk: false
    , edit: false
    }

  render :: NLState -> H.ComponentHTML NLAction () Aff
  render st@{ notes } =
    HH.div_ (map renderNote notes)
    where
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

  handleAction :: NLAction -> H.HalogenM NLState NLAction () o Aff Unit
  handleAction NLNop = pure unit
