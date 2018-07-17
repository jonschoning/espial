module Component.BMark where

import Prelude

import Control.Monad.State as CMS
import Data.Array (drop, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.String (null, split) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst, snd)
import Globals (app', mmoment8601)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model (Bookmark)

-- | The bookmark component query algebra.
data BQuery a
  = BToggle Boolean a
  | BRemove a

data BMessage
  = BNotifyRemove

-- | The bookmark component definition.
bmark :: forall m. Bookmark -> H.Component HH.HTML BQuery Unit BMessage m
bmark initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  app = app' unit

  render :: Bookmark -> H.ComponentHTML BQuery
  render b =
    HH.div_ ([ bookmark ] <> clearBoth)
   where
     bookmark =
       HH.div
          [ HP.id_ (show b.id)
          , HP.class_ $ ClassName ("bookmark" <> (if b.shared then "" else " private"))
          ]
          $  star
          <> display
          <> clearBoth

     star =
       if app.dat.isowner then
         [ HH.div
           [ HP.class_ $ ClassName ("star" <> if b.selected then " selected_star" else "")]
           [ HH.span [] [ HH.text "✭" ] ]
         ]
       else
         []

     display =
       [ HH.div [ HP.class_ $ ClassName "display"] $
         [ HH.a
           [ HP.href b.href, HP.target "_blank", HP.class_ $ ClassName ("bookmark_title" <> if b.toRead then " unread" else "")]
           [ HH.text $ if S.null b.description then "[no title]" else b.description ]
         , HH.br_
         , HH.a [ HP.href b.href , HP.class_ $ ClassName "url_display" ] [ HH.text b.href ]
         , HH.br_
         , HH.div [ HP.class_ $ ClassName "description" ] (toTextarea b.extended)
         , HH.div [ HP.class_ $ ClassName "tags" ] [ ]
         , HH.a
           [ HP.class_ $ ClassName "when js-moment"
           , HP.title $ maybe b.time snd mmoment
           , HP.attr (HH.AttrName "data-iso8601") b.time
           , HP.href $ linkToFilterSingle b.id
           ]
           [ HH.text $ maybe " " fst mmoment ]
         ]
         <> links
       ]

     links =
       if app.dat.isowner then
         [ HH.div_ [ HH.text "[links]"]
         ]
       else
         []

     linkToFilterSingle bid = app.userR <> "/b:" <> show bid
     mmoment = mmoment8601 b.time
     clearBoth = [ HH.div [ HP.attr (AttrName "style") "clear:both" ] [] ]
     toTextarea =
       drop 1
         <<< foldMap (\x -> [HH.br_, HH.text x])
         <<< S.split (Pattern "\n")

  eval :: BQuery ~> H.ComponentDSL Bookmark BQuery BMessage m
  eval (BToggle b next) = do
    pure next
  eval (BRemove next) = do
    H.raise BNotifyRemove
    pure next
