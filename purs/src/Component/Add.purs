module Component.Add where

import Prelude hiding (div)

import App (destroy, editBookmark)
import Data.Array (drop, foldMap)
import Data.Lens (Lens', lens, use, (%=), (.=))
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
import Halogen.HTML (HTML, br_, button, div, div_, form, input, label, p, span, table, tbody_, td, td_, text, textarea, tr_)
import Halogen.HTML.Events (onSubmit, onValueChange, onChecked, onClick)
import Halogen.HTML.Properties (autofocus, ButtonType(..), InputType(..), autocomplete, checked, for, id_, name, required, rows, title, type_, value)
import Model (Bookmark)
import Util (_curQuerystring, _loc, _lookupQueryStringValue, attr, class_)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Location (setHref)

data BAction
  = BEditField EditField
  | BEditSubmit Event
  | BDeleteAsk Boolean
  | BDestroy

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

_bm :: Lens' BState Bookmark
_bm = lens _.bm (_ { bm = _ })

_edit_bm :: Lens' BState Bookmark
_edit_bm = lens _.edit_bm (_ { edit_bm = _ })

addbmark :: forall q i o. Bookmark -> H.Component HTML q i o Aff
addbmark b' =
  H.mkComponent
    { initialState: const (mkState b')
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  app = app' unit

  mkState b =
    { bm: b
    , edit_bm: b
    , deleteAsk: false
    , destroyed: false
    }

  render :: forall m. BState -> H.ComponentHTML BAction () m
  render s@{ bm, edit_bm } =
    div_ [ if not s.destroyed then display_edit else display_destroyed ]
   where
     display_edit =
       form [ onSubmit (Just <<< BEditSubmit) ]
       [ table [ class_ "w-100" ]
         [ tbody_
           [ tr_
             [ td [ class_ "w1" ] [ ]
             , td_ $ guard (bm.bid > 0) [ display_exists ]
             ]
           , tr_
             [ td_ [ label [ for "url" ] [ text "URL" ] ]
             , td_ [ input [ type_ InputUrl , id_ "url", class_ "w-100 mv1" , required true, name "url", autofocus (null bm.url)
                           , value (edit_bm.url) , onValueChange (editField Eurl)] ]
             ]
           , tr_
             [ td_ [ label [ for "title" ] [ text "title" ] ]
             , td_ [ input [ type_ InputText , id_ "title", class_ "w-100 mv1" , name "title"
                           , value (edit_bm.title) , onValueChange (editField Etitle)] ]
             ]
           , tr_
             [ td_ [ label [ for "description" ] [ text "description" ] ]
             , td_ [ textarea [ class_ "w-100 mt1 mid-gray" , id_ "description", name "description", rows 4
                              , value (edit_bm.description) , onValueChange (editField Edescription)] ]
             ]
           , tr_
             [ td_ [ label [ for "tags" ] [ text "tags" ] ]
             , td_ [ input [ type_ InputText , id_ "tags", class_ "w-100 mv1" , name "tags", autocomplete false, attr "autocapitalize" "off", autofocus (not $ null bm.url)
                           , value (edit_bm.tags) , onValueChange (editField Etags)] ]
             ]
           , tr_
             [ td_ [ label [ for "private" ] [ text "private" ] ]
             , td_ [ input [ type_ InputCheckbox , id_ "private", class_ "private pointer" , name "private"
                           , checked (edit_bm.private) , onChecked (editField Eprivate)] ]
             ]
           , tr_
             [ td_ [ label [ for "toread" ] [ text "read later" ] ]
             , td_ [ input [ type_ InputCheckbox , id_ "toread", class_ "toread pointer" , name "toread"
                           , checked (edit_bm.toread) , onChecked (editField Etoread)] ]
             ]
           , tr_
             [ td_ [ ]
             , td_ [ input [ type_ InputSubmit , class_ "ph3 pv2 input-reset ba b--navy bg-transparent pointer f6 dib mt1 dim"
                           , value (if bm.bid > 0 then "update bookmark" else "add bookmark") ] ]
             ]
           ]
         ]
       ]

     display_exists = 
       div [ class_ "alert" ]
       [ text "previously saved "
       , span [ class_ "link f7 dib gray pr3" , title (maybe bm.time snd mmoment) ]
         [ text (maybe " " fst mmoment) ]
       , div [ class_ "edit_links dib ml1" ]
         [ div [ class_ "delete_link di" ]
           [ button ([ type_ ButtonButton, onClick \_ -> Just (BDeleteAsk true), class_ "delete" ] <> guard s.deleteAsk [ attr "hidden" "hidden" ]) [ text "delete" ]
           , span ([ class_ "confirm red" ] <> guard (not s.deleteAsk) [ attr "hidden" "hidden" ])
             [ button [ type_ ButtonButton, onClick \_ -> Just (BDeleteAsk false)] [ text "cancel / " ]
             , button [ type_ ButtonButton, onClick \_ -> Just BDestroy, class_ "red" ] [ text "destroy" ]
             ] 
           ]
         ]
       ]

     display_destroyed = p [ class_ "red"] [text "you killed this bookmark"]

     editField :: forall a. (a -> EditField) -> a -> Maybe BAction
     editField f = Just <<< BEditField <<< f
     mmoment = mmoment8601 bm.time
     toTextarea =
       drop 1
         <<< foldMap (\x -> [br_, text x])
         <<< S.split (Pattern "\n")

  handleAction :: BAction -> H.HalogenM BState BAction () o Aff Unit
  handleAction (BDeleteAsk e) = do
    H.modify_ (_ { deleteAsk = e })
  handleAction (BDestroy) = do
    bid <- H.gets _.bm.bid
    void $ H.liftAff (destroy bid)
    H.modify_ (_ { destroyed = true })
  handleAction (BEditField f) = do
    _edit_bm %= case f of
      Eurl e -> _ { url = e }
      Etitle e -> _ { title = e }
      Edescription e -> _ { description = e }
      Etags e -> _ { tags = e }
      Eprivate e -> _ { private = e }
      Etoread e -> _ { toread = e }
  handleAction (BEditSubmit e) = do
    H.liftEffect (preventDefault e)
    edit_bm <- use _edit_bm 
    void $ H.liftAff (editBookmark edit_bm)
    _bm .= edit_bm
    loc <- liftEffect _loc
    win <- liftEffect window
    qs <- liftEffect _curQuerystring
    case _lookupQueryStringValue qs "next" of
      Just n -> liftEffect (setHref n loc)
      _ -> liftEffect (closeWindow win)
