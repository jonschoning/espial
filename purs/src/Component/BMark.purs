module Component.BMark where

import Prelude

import Control.Monad.State as CMS

import Data.Maybe (Maybe(..))
import Data.String (null) as S
import Globals (app')
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
          (  star
          <> display
          <> clearBoth
          )

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
         ] <> links
       ]

     links =
       if app.dat.isowner then
         [ HH.div_ [ HH.text "[links]"]
         ]
       else
         []

     clearBoth = [ HH.div [ HP.attr (AttrName "style") "clear:both" ] []]

  eval :: BQuery ~> H.ComponentDSL Bookmark BQuery BMessage m
  eval (BToggle b next) = do
    pure next
  eval (BRemove next) = do
    H.raise BNotifyRemove
    pure next
