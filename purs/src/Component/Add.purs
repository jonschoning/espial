module Component.Add where

import Prelude hiding (div)

import App (destroy, editBookmark)
import Control.Monad.State.Class (class MonadState)
import Data.Array (drop, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String (null)
import Data.String (split) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Globals (app', closeWindow, mmoment8601)
import Halogen as H
import Halogen.HTML (HTML, br_, button, div, div_, form, input, label, p, span, table_, tbody_, td_, text, textarea, tr_)
import Halogen.HTML.Events (onSubmit, onValueChange, onChecked, onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (autofocus, ButtonType(..), InputType(..), autocomplete, checked, for, id_, name, required, rows, title, type_, value)
import Model (Bookmark)
import Util (_curQuerystring, _loc, _lookupQueryStringValue, attr, class_)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location (setHref)

-- | The bookmark component query algebra.
data BQuery a
  = BEditField EditField a
  | BEditSubmit Event a
  | BDeleteAsk Boolean a
  | BDestroy a

data EditField
  = Eurl String
  | Etitle String
  | Edescription String
  | Etags String
  | Eprivate Boolean
  | Etoread Boolean

type BState =
  { bm :: Bookmark
  , edit_bm :: Bookmark
  , deleteAsk :: Boolean
  , destroyed :: Boolean
  }

-- | The bookmark component definition.
addbmark :: Bookmark -> H.Component HTML BQuery Unit Unit Aff
addbmark b' =
  H.component
    { initialState: const (mkState b')
    , render
    , eval
    , receiver: const Nothing
    }
  where
  app = app' unit

  mkState b =
    { bm: b
    , edit_bm: b
    , deleteAsk: false
    , destroyed: false
    }

  render :: BState -> H.ComponentHTML BQuery
  render s@{ bm, edit_bm } =
    div_ [ if not s.destroyed then display_edit else display_destroyed ]
   where
     display_edit =
       form [ onSubmit (HE.input BEditSubmit) ]
       [ table_
         [ tbody_
           [ tr_
             [ td_ [ ]
             , td_ $ guard (bm.bid > 0) [ display_exists ]
             ]
           , tr_
             [ td_ [ label [ for "url" ] [ text "URL" ] ]
             , td_ [ input [ type_ InputUrl , id_ "url", class_ "url" , required true, name "url", attr "size" "70", autofocus (null bm.url)
                           , value (edit_bm.url) , onValueChange (HE.input BEditField <<< Eurl)] ]
             ]
           , tr_
             [ td_ [ label [ for "title" ] [ text "title" ] ]
             , td_ [ input [ type_ InputText , id_ "title", class_ "title" , name "title", attr "size" "70"
                           , value (edit_bm.title) , onValueChange (HE.input BEditField <<< Etitle)] ]
             ]
           , tr_
             [ td_ [ label [ for "description" ] [ text "description" ] ]
             , td_ [ textarea [ class_ "description" , id_ "description", name "description", attr "cols" "70", rows 4
                              , value (edit_bm.description) , onValueChange (HE.input BEditField <<< Edescription)] ]
             ]
           , tr_
             [ td_ [ label [ for "tags" ] [ text "tags" ] ]
             , td_ [ input [ type_ InputText , id_ "tags", class_ "tags" , name "tags", attr "size" "70", autocomplete false, attr "autocapitalize" "off", autofocus (not $ null bm.url)
                           , value (edit_bm.tags) , onValueChange (HE.input BEditField <<< Etags)] ]
             ]
           , tr_
             [ td_ [ label [ for "private" ] [ text "private" ] ]
             , td_ [ input [ type_ InputCheckbox , id_ "private", class_ "private" , name "private"
                           , checked (edit_bm.private) , onChecked (HE.input BEditField <<< Eprivate)] ]
             ]
           , tr_
             [ td_ [ label [ for "toread" ] [ text "read later" ] ]
             , td_ [ input [ type_ InputCheckbox , id_ "toread", class_ "toread" , name "toread"
                           , checked (edit_bm.toread) , onChecked (HE.input BEditField <<< Etoread)] ]
             ]
           , tr_
             [ td_ [ ]
             , td_ [ input [ type_ InputSubmit , class_ "submit"
                           , value (if bm.bid > 0 then "update bookmark" else "add bookmark") ] ]
             ]
           ]
         ]
       ]

     display_exists = 
       div [ class_ "alert" ]
       [ text "previously saved "
       , span [ class_ "when js-moment" , title (maybe bm.time snd mmoment) , attr "data-iso8601" bm.time]
         [ text (maybe " " fst mmoment) ]
       , div [ class_ "edit_links", attr "style" "display: inline-block; margin-left: 5px;"]
         [ div [ class_ "delete_link" ]
           [ button ([ type_ ButtonButton, onClick (HE.input_ (BDeleteAsk true)), class_ "delete" ] <> guard s.deleteAsk [ attr "hidden" "hidden" ]) [ text "delete" ]
           , span ([ class_ "confirm" ] <> guard (not s.deleteAsk) [ attr "hidden" "hidden" ])
             [ button [ type_ ButtonButton, onClick (HE.input_ (BDeleteAsk false))] [ text "cancel / " ]
             , button [ type_ ButtonButton, onClick (HE.input_ BDestroy), class_ "destroy" ] [ text "destroy" ]
             ] 
           ]
         ]
       ]

     display_destroyed = p [ class_ "error"] [text "you killed this bookmark"]

     mmoment = mmoment8601 bm.time
     toTextarea =
       drop 1
         <<< foldMap (\x -> [br_, text x])
         <<< S.split (Pattern "\n")

  eval :: BQuery ~> H.ComponentDSL BState BQuery Unit Aff
  eval (BDeleteAsk e next) = do
    H.modify_ (_ { deleteAsk = e })
    pure next
  eval (BDestroy next) = do
    bid <- H.gets _.bm.bid
    void $ H.liftAff (destroy bid)
    H.modify_ (_ { destroyed = true })
    pure next
  eval (BEditField f next) = do
    modifyEdit $ case f of
      Eurl x -> _ { url = x }
      Etitle x -> _ { title = x }
      Edescription x -> _ { description = x }
      Etags x -> _ { tags = x }
      Eprivate x -> _ { private = x }
      Etoread x -> _ { toread = x }
    pure next
    where
      modifyEdit :: forall m. MonadState BState m => (Bookmark -> Bookmark) -> m Unit
      modifyEdit g = H.modify_ \s -> s { edit_bm = g s.edit_bm  }
  eval (BEditSubmit e next) = do
    H.liftEffect (preventDefault e)
    s <- H.get
    void $ H.liftAff (editBookmark s.edit_bm)
    H.put $ s { bm = s.edit_bm }
    loc <- liftEffect _loc
    win <- liftEffect window
    qs <- liftEffect _curQuerystring
    case _lookupQueryStringValue qs "next" of
      Just n -> liftEffect (setHref n loc)
      _ -> liftEffect (closeWindow win)
    pure next
