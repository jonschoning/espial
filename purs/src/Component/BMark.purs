module Component.BMark where

import Prelude

import App (StarAction(..), destroy, fetchUrlEnc, markRead, toggleStar)
import Data.Array (drop, foldMap)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String (null, split, take) as S
import Data.String.Pattern (Pattern(..))
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Globals (app', mmoment8601)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Model (Bookmark)
import Web.Event.Event (Event, preventDefault)
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax.Response as AXRes

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
bmark :: Bookmark -> H.Component HH.HTML BQuery Unit BMessage Aff
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
    HH.div_ ([ bookmark ] <> clearBoth)
   where
     bookmark =
       HH.div
          [ HP.id_ (show bm.bid)
          , HP.class_ $ ClassName ("bookmark" <> (if bm.private then " private" else ""))
          ]
          $  star
          <> if s.edit then display_edit else display
          <> clearBoth

     star =
       if app.dat.isowner then
         [ HH.div [ HP.class_ $ ClassName ("star" <> if bm.selected then " selected_star" else "")]
           [ HH.button [ HE.onClick $ HE.input_ $ BStar $ not bm.selected ] [ HH.text "✭" ] ]
         ]
       else
         []

     display =
       [ HH.div [ HP.class_ (ClassName "display") ] $
         [ HH.a [ HP.href bm.url, HP.target "_blank", HP.class_ $ ClassName ("bookmark_title" <> if bm.toread then " unread" else "")]
           [ HH.text $ if S.null bm.title then "[no title]" else bm.title ]
         , HH.br_
         , HH.a [ HP.href bm.url , HP.class_ (ClassName "url_display") ] [ HH.text bm.url ]
         , HH.br_
         , HH.div [ HP.class_ (ClassName "description") ] (toTextarea bm.description)
         , HH.div [ HP.class_ (ClassName "tags") ] $
             if S.null bm.tags
             then []
             else 
               map (\tag -> HH.a
                            [ HP.class_ $ ClassName $ "tag" <> if (S.take 1 tag == ".") then " private" else ""
                            , HP.href (linkToFilterTag tag)
                            ]
                            [ HH.text tag ])
               (S.split (Pattern " ") bm.tags)
         , HH.a [ HP.class_ (ClassName "when js-moment")
                , HP.title (maybe bm.time snd mmoment)
                , HP.attr (HH.AttrName "data-iso8601") bm.time
                , HP.href (linkToFilterSingle bm.bid)
                ]
           [ HH.text (maybe " " fst mmoment) ]
         ]
         <> links
       ]

     display_edit =
       [ HH.div [ HP.class_ (ClassName "edit_bookmark_form active") ] $
         [ HH.form [ HE.onSubmit (HE.input BEditSubmit) ]
           [ HH.input [ HP.type_ HP.InputUrl , HP.class_ (ClassName "url edit_form_input") , HP.required true , HP.name "url"
             , HP.value (edit_bm.url) , HE.onValueChange (HE.input BEditField <<< Eurl)
             ]
           , HH.br_
           , HH.input [ HP.type_ HP.InputText , HP.class_ (ClassName "title edit_form_input") , HP.name "title"
             , HP.value (edit_bm.title) , HE.onValueChange (HE.input BEditField <<< Etitle)
             ]
           , HH.br_
           , HH.p [ HP.attr (AttrName "style") "margin-top:10px;margin-bottom:5px;" ] [ HH.text " description:" ]
           , HH.textarea [ HP.class_ (ClassName "description edit_form_input") , HP.name "description", HP.rows 5
             , HP.value (edit_bm.description) , HE.onValueChange (HE.input BEditField <<< Edescription)
             ]
           , HH.br_
           , HH.div [ HP.id_ "tags_input_box"]
             [ HH.p [ HP.attr (AttrName "style") "margin-top:10px" ]
               [ HH.text " tags:"
               , HH.input [ HP.type_ HP.InputText , HP.class_ (ClassName "tags edit_form_input") , HP.name "tags"
                 , HP.autocomplete false, HP.attr (AttrName "autocapitalize") "off"
                 , HP.value (edit_bm.tags) , HE.onValueChange (HE.input BEditField <<< Etags)
                 ]
               , HH.br_
               ]
             ]
           , HH.div [ HP.id_ "edit_form_checkboxes"]
             [ HH.input [ HP.type_ HP.InputCheckbox , HP.class_ (ClassName "private") , HP.id_ "edit_private", HP.name "private"
               , HP.checked (edit_bm.private) , HE.onChecked (HE.input BEditField <<< Eprivate) ]
             , HH.text " "
             , HH.label [ HP.for "edit_private" ] [ HH.text "private" ]
             , HH.text " "
             , HH.input [ HP.type_ HP.InputCheckbox , HP.class_ (ClassName "toread") , HP.id_ "edit_toread", HP.name "toread"
               , HP.checked (edit_bm.toread) , HE.onChecked (HE.input BEditField <<< Etoread) ]
             , HH.text " "
             , HH.label [ HP.for "edit_toread" ] [ HH.text "toread" ]
             , HH.br_
             ]
           , HH.input [ HP.type_ HP.InputSubmit , HP.class_ (ClassName "submit edit-button") , HP.value "save" ]
           , HH.text " "
           , HH.input [ HP.type_ HP.InputReset , HP.class_ (ClassName "reset edit-button") , HP.value "cancel"
             , HE.onClick (HE.input_ (BEdit false))
             ]
           ]
         ]
       ]

     links =
       if not app.dat.isowner then [] else
         [ HH.div [ HP.class_ (ClassName "edit_links") ]
           [ HH.button [ HE.onClick (HE.input_ (BEdit true)), HP.class_ (ClassName "edit") ] [ HH.text "edit  " ]
           , HH.div [ HP.class_ (ClassName "delete_link") ]
             [ HH.button ([ HE.onClick (HE.input_ (BDeleteAsk true)), HP.class_ (ClassName "delete") ] <> if s.deleteAsk then [ HP.attr (AttrName "hidden") "hidden" ] else []) [ HH.text "delete" ]
             , HH.span ([ HP.class_ (ClassName "confirm") ] <> if not s.deleteAsk then [ HP.attr (AttrName "hidden") "hidden" ] else [])
               [ HH.button [ HE.onClick (HE.input_ (BDeleteAsk false))] [ HH.text "cancel / " ]
               , HH.button [ HE.onClick (HE.input_ BDestroy), HP.class_ (ClassName "destroy") ] [ HH.text "destroy" ]
               ] 
             ]
           ]
         , HH.div [ HP.class_ (ClassName "read") ]
             if not bm.toread then [] else
             [ HH.text "  "
             , HH.button [ HE.onClick (HE.input_ BMarkRead), HP.class_ (ClassName "mark_read") ] [ HH.text "mark as read"]
             ]
         ]

     linkToFilterSingle bid = app.userR <> "/b:" <> show bid
     linkToFilterTag tag = app.userR <> "/t:" <> tag
     mmoment = mmoment8601 bm.time
     clearBoth = [ HH.div [ HP.attr (AttrName "style") "clear:both" ] [] ]
     toTextarea =
       drop 1
         <<< foldMap (\x -> [HH.br_, HH.text x])
         <<< S.split (Pattern "\n")

  eval :: BQuery ~> H.ComponentDSL BState BQuery BMessage Aff
  eval (BStar e next) = do
    s <- H.get
    H.liftAff $ toggleStar s.bm.bid (if e then Star else UnStar)
    H.put $ s { bm = s.bm { selected = e } }
    pure next
  eval (BDeleteAsk e next) = do
    H.modify_ (_ { deleteAsk = e })
    pure next
  eval (BDestroy next) = do
    bid <- H.gets _.bm.bid
    void $ H.liftAff $ destroy bid
    H.raise BNotifyRemove
    pure next
  eval (BMarkRead next) = do
    s <- H.get
    void $ H.liftAff $ markRead s.bm.bid
    H.put $ s { bm = s.bm { toread = false } }
    pure next
  eval (BEdit e next) = do
    s <- H.get
    H.put $ s { edit = e, edit_bm = s.bm }
    pure next
  eval (BEditField f next) = do
    s <- H.get
    case f of
      Eurl e -> H.put $ s { edit_bm = s.edit_bm { url = e } }
      Etitle e -> H.put $ s { edit_bm = s.edit_bm { title = e } }
      Edescription e -> H.put $ s { edit_bm = s.edit_bm { description = e } }
      Etags e -> H.put $ s { edit_bm = s.edit_bm { tags = e } }
      Eprivate e -> H.put $ s { edit_bm = s.edit_bm { private = e } }
      Etoread e -> H.put $ s { edit_bm = s.edit_bm { toread = e } }
    pure next
  eval (BEditSubmit e next) = do
    H.liftEffect $ preventDefault e
    s <- H.get
    let dat = Just $ FormURLEncoded $ []
    void $ H.liftAff $ fetchUrlEnc POST "add?inline=true" dat AXRes.ignore
    H.put $ s { edit = false, bm = s.edit_bm }
    pure next
