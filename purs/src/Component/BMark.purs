module Component.BMark where

import Prelude hiding (div)

import App (StarAction(..), destroy, editBookmark, markRead, toggleStar)
import Control.Monad.State.Class (class MonadState)
import Data.Array (drop, foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.String (null, split, take) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Globals (app', mmoment8601)
import Halogen as H
import Halogen.HTML (HTML, a, br_, button, div, div_, form, input, label, p, span, text, textarea)
import Halogen.HTML.Events (onSubmit, onValueChange, onChecked, onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..), autocomplete, checked, for, href, id_, name, required, rows, target, title, type_, value)
import Model (Bookmark)
import Util (class_, attr)
import Web.Event.Event (Event, preventDefault)

-- | The bookmark component query algebra.
data BQuery a
  = BStar Boolean a
  | BDeleteAsk Boolean a
  | BDestroy a
  | BEdit Boolean a
  | BEditField EditField a
  | BEditSubmit Event a
  | BMarkRead a

data EditField
  = Eurl String
  | Etitle String
  | Edescription String
  | Etags String
  | Eprivate Boolean
  | Etoread Boolean

data BMessage
  = BNotifyRemove

type BState =
  { bm :: Bookmark
  , edit_bm :: Bookmark
  , deleteAsk:: Boolean
  , edit :: Boolean
  }

-- | The bookmark component definition.
bmark :: Bookmark -> H.Component HTML BQuery Unit BMessage Aff
bmark b' =
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
    , edit: false
    }

  render :: BState -> H.ComponentHTML BQuery
  render s@{ bm, edit_bm } =
    div_ ([ bookmark ] <> clearBoth)
   where
     bookmark =
       div
          [ id_ (show bm.bid)
          , class_ ("bookmark" <> guard bm.private " private")
          ]
          $  star
          <> if s.edit then display_edit else display
          <> clearBoth

     star =
       guard app.dat.isowner
         [ div [ class_ ("star" <> guard bm.selected " selected_star") ]
           [ button [ onClick (HE.input_ (BStar (not bm.selected))) ] [ text "✭" ] ]
         ]

     display =
       [ div [ class_ "display" ] $
         [ a [ href bm.url, target "_blank", class_ ("bookmark_title" <> guard bm.toread " unread")]
           [ text $ if S.null bm.title then "[no title]" else bm.title ]
         , br_
         , a [ href bm.url , class_ "url_display" ] [ text bm.url ]
         , br_
         , div [ class_ "description" ] (toTextarea bm.description)
         , div [ class_ "tags" ] $
             guard (not (S.null bm.tags))
               map (\tag -> a [ class_ ("tag" <> guard (S.take 1 tag == ".") " private")
                            , href (linkToFilterTag tag) ]
                            [ text tag ])
               (S.split (Pattern " ") bm.tags)
         , a [ class_ "when js-moment"
           , title (maybe bm.time snd mmoment)
           , attr "data-iso8601" bm.time
           , href (linkToFilterSingle bm.bid) ]
           [ text (maybe " " fst mmoment) ]
         ]
         <> links
       ]

     display_edit =
       [ div [ class_ "edit_bookmark_form active" ] $
         [ form [ onSubmit (HE.input BEditSubmit) ]
           [ input [ type_ InputUrl , class_ "url edit_form_input" , required true , name "url"
             , value (edit_bm.url) , onValueChange (HE.input BEditField <<< Eurl)
             ]
           , br_
           , input [ type_ InputText , class_ "title edit_form_input" , name "title"
             , value (edit_bm.title) , onValueChange (HE.input BEditField <<< Etitle)
             ]
           , br_
           , p [ attr "style" "margin-top:10px;margin-bottom:5px;" ] [ text " description:" ]
           , textarea [ class_ "description edit_form_input" , name "description", rows 5
             , value (edit_bm.description) , onValueChange (HE.input BEditField <<< Edescription)
             ]
           , br_
           , div [ id_ "tags_input_box"]
             [ p [ attr "style" "margin-top:10px" ]
               [ text " tags:"
               , input [ type_ InputText , class_ "tags edit_form_input" , name "tags"
                 , autocomplete false, attr "autocapitalize" "off"
                 , value (edit_bm.tags) , onValueChange (HE.input BEditField <<< Etags)
                 ]
               , br_
               ]
             ]
           , div [ id_ "edit_form_checkboxes"]
             [ input [ type_ InputCheckbox , class_ "private" , id_ "edit_private", name "private"
               , checked (edit_bm.private) , onChecked (HE.input BEditField <<< Eprivate) ]
             , text " "
             , label [ for "edit_private" ] [ text "private" ]
             , text " "
             , input [ type_ InputCheckbox , class_ "toread" , id_ "edit_toread", name "toread"
               , checked (edit_bm.toread) , onChecked (HE.input BEditField <<< Etoread) ]
             , text " "
             , label [ for "edit_toread" ] [ text "toread" ]
             , br_
             ]
           , input [ type_ InputSubmit , class_ "submit edit-button" , value "save" ]
           , text " "
           , input [ type_ InputReset , class_ "reset edit-button" , value "cancel"
             , onClick (HE.input_ (BEdit false))
             ]
           ]
         ]
       ]

     links =
       guard app.dat.isowner
         [ div [ class_ "edit_links" ]
           [ button [ type_ ButtonButton, onClick (HE.input_ (BEdit true)), class_ "edit" ] [ text "edit  " ]
           , div [ class_ "delete_link" ]
             [ button ([ type_ ButtonButton, onClick (HE.input_ (BDeleteAsk true)), class_ "delete" ] <> guard s.deleteAsk [ attr "hidden" "hidden" ]) [ text "delete" ]
             , span ([ class_ "confirm" ] <> guard (not s.deleteAsk) [ attr "hidden" "hidden" ])
               [ button [ type_ ButtonButton, onClick (HE.input_ (BDeleteAsk false))] [ text "cancel / " ]
               , button [ type_ ButtonButton, onClick (HE.input_ BDestroy), class_ "destroy" ] [ text "destroy" ]
               ] 
             ]
           ]
         , div [ class_ "read" ] $
             guard bm.toread
             [ text "  "
             , button [ onClick (HE.input_ BMarkRead), class_ "mark_read" ] [ text "mark as read"]
             ]
         ]

     linkToFilterSingle bid = app.userR <> "/b:" <> show bid
     linkToFilterTag tag = app.userR <> "/t:" <> tag
     mmoment = mmoment8601 bm.time
     clearBoth = [ div [ attr "style" "clear:both" ] [] ]
     toTextarea =
       drop 1
         <<< foldMap (\x -> [br_, text x])
         <<< S.split (Pattern "\n")

  eval :: BQuery ~> H.ComponentDSL BState BQuery BMessage Aff
  eval (BStar e next) = do
    s <- H.get
    H.liftAff $ toggleStar s.bm.bid (if e then Star else UnStar)
    H.put $ s { bm = s.bm { selected = e }, edit_bm = s.edit_bm { selected = e } }
    pure next
  eval (BDeleteAsk e next) = do
    H.modify_ (_ { deleteAsk = e })
    pure next
  eval (BDestroy next) = do
    bid <- H.gets _.bm.bid
    void $ H.liftAff (destroy bid)
    H.raise BNotifyRemove
    pure next
  eval (BMarkRead next) = do
    s <- H.get
    void $ H.liftAff (markRead s.bm.bid)
    H.put $ s { bm = s.bm { toread = false } }
    pure next
  eval (BEdit e next) = do
    s <- H.get
    H.put $ s { edit = e, edit_bm = s.bm }
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
    H.put $ s { edit = false, bm = s.edit_bm }
    pure next
