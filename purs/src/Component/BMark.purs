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
import Halogen.HTML (HTML, a, br_, button, div, div_, form, input, label, p, p_, span, text, textarea)
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
    div [ id_ (show bm.bid) , class_ ("bookmark w-100 mw7 pa1 mb3" <> guard bm.private " private")] $
      star <>
      if s.edit
        then display_edit
        else display
    where

     star =
       guard app.dat.isowner
         [ div [ class_ ("star fl pointer" <> guard bm.selected " selected") ]
           [ button [ class_ "moon-gray", onClick (HE.input_ (BStar (not bm.selected))) ] [ text "✭" ] ]
         ]

     display =
       [ div [ class_ "display" ] $
         [ a [ href bm.url, target "_blank", class_ ("link f6 lh-title" <> guard bm.toread " unread")]
           [ text $ if S.null bm.title then "[no title]" else bm.title ]
         , br_
         , a [ href bm.url , class_ "link f7 gray hover-blue" ] [ text bm.url ]
         , br_
         , div [ class_ "description mt1 mid-gray" ] (toTextarea bm.description)
         , div [ class_ "tags" ] $
             guard (not (S.null bm.tags))
               map (\tag -> a [ class_ ("link tag mr1" <> guard (S.take 1 tag == ".") " private")
                            , href (linkToFilterTag tag) ]
                            [ text tag ])
               (S.split (Pattern " ") bm.tags)
         , a [ class_ "link f7 dib gray w4", title (maybe bm.time snd mmoment) , href (linkToFilterSingle bm.bid) ]
           [ text (maybe " " fst mmoment) ]
         ]
         <> links
       ]

     display_edit =
       [ div [ class_ "edit_bookmark_form pa2 bg-white" ] $
         [ form [ onSubmit (HE.input BEditSubmit) ]
           [ input [ type_ InputUrl , class_ "url w-100 mt2 mb1 pt1 f7 edit_form_input" , required true , name "url"
             , value (edit_bm.url) , onValueChange (editField Eurl)
             ]
           , br_
           , input [ type_ InputText , class_ "title w-100 mt2 mb1 pt1 f7 edit_form_input" , name "title"
             , value (edit_bm.title) , onValueChange (editField Etitle)
             ]
           , br_
           , p [ class_ "mt2 mb1"] [ text " description:" ]
           , textarea [ class_ "description w-100 mt2 mb1 pt1 f7 edit_form_input" , name "description", rows 5
             , value (edit_bm.description) , onValueChange (editField Edescription)
             ]
           , br_
           , div [ id_ "tags_input_box"]
             [ p_ 
               [ div_ [ text " tags:" ]
               , input [ type_ InputText , class_ "tags w-100 mt2 mb1 pt1 f7 edit_form_input" , name "tags"
                 , autocomplete false, attr "autocapitalize" "off"
                 , value (edit_bm.tags) , onValueChange (editField Etags)
                 ]
               , br_
               ]
             ]
           , div [ class_ "edit_form_checkboxes mv3"]
             [ input [ type_ InputCheckbox , class_ "private pointer" , id_ "edit_private", name "private"
               , checked (edit_bm.private) , onChecked (editField Eprivate) ]
             , text " "
             , label [ for "edit_private" , class_ "mr2" ] [ text "private" ]
             , text " "
             , input [ type_ InputCheckbox , class_ "toread pointer" , id_ "edit_toread", name "toread"
               , checked (edit_bm.toread) , onChecked (editField Etoread) ]
             , text " "
             , label [ for "edit_toread" ] [ text "toread" ]
             , br_
             ]
           , input [ type_ InputSubmit , class_ "mr1 pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "save" ]
           , text " "
           , input [ type_ InputReset , class_ "pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "cancel"
             , onClick (HE.input_ (BEdit false))
             ]
           ]
         ]
       ]

     links =
       guard app.dat.isowner
         [ div [ class_ "edit_links di" ]
           [ button [ type_ ButtonButton, onClick (HE.input_ (BEdit true)), class_ "edit light-silver hover-blue" ] [ text "edit  " ]
           , div [ class_ "delete_link di" ]
             [ button [ type_ ButtonButton, onClick (HE.input_ (BDeleteAsk true)), class_ ("delete light-silver hover-blue" <> guard s.deleteAsk " dn") ] [ text "delete" ]
             , span ([ class_ ("confirm red" <> guard (not s.deleteAsk) " dn") ] )
               [ button [ type_ ButtonButton, onClick (HE.input_ (BDeleteAsk false))] [ text "cancel / " ]
               , button [ type_ ButtonButton, onClick (HE.input_ BDestroy), class_ "red" ] [ text "destroy" ]
               ] 
             ]
           ]
         , div [ class_ "read di" ] $
             guard bm.toread
             [ text "  "
             , button [ onClick (HE.input_ BMarkRead), class_ "mark_read" ] [ text "mark as read"]
             ]
         ]

     editField :: forall a. (a -> EditField) -> a -> Maybe (BQuery Unit)
     editField f = HE.input BEditField <<< f
     linkToFilterSingle bid = app.userR <> "/b:" <> show bid
     linkToFilterTag tag = app.userR <> "/t:" <> tag
     mmoment = mmoment8601 bm.time
     toTextarea input =
       S.split (Pattern "\n") input
       # foldMap (\x -> [br_, text x])
       # drop 1

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
      Eurl e -> _ { url = e }
      Etitle e -> _ { title = e }
      Edescription e -> _ { description = e }
      Etags e -> _ { tags = e }
      Eprivate e -> _ { private = e }
      Etoread e -> _ { toread = e }
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
