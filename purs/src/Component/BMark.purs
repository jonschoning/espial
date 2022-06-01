module Component.BMark where

import Prelude hiding (div)

import Affjax (printError)
import Affjax.StatusCode (StatusCode(..))
import App (StarAction(..), destroy, editBookmark, markRead, toggleStar, lookupTitle)
import Component.Markdown as Markdown
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Lens (Lens', lens, use, (%=), (.=))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.String (null, split, take, replaceAll) as S
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Globals (app', setFocus, toLocaleDateString)
import Halogen as H
import Halogen.HTML (a, br_, button, div, div_, form, input, label, span, text, textarea)
import Halogen.HTML as HH
import Halogen.HTML.Events (onSubmit, onValueChange, onChecked, onClick)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), autocomplete, checked, disabled, for, href, id, name, required, rows, target, title, type_, value)
import Model (Bookmark)
import Type.Proxy (Proxy(..))
import Util (attr, class_, encodeTag, fromNullableStr, ifElseH, whenA, whenH)
import Web.Event.Event (Event, preventDefault)
import DOM.HTML.Indexed.AutocompleteType (AutocompleteType(..))

-- | UI Events
data BAction
  = BStar Boolean
  | BDeleteAsk Boolean
  | BLookupTitle
  | BDestroy
  | BEdit Boolean
  | BEditField EditField
  | BEditSubmit Event
  | BMarkRead

-- | FormField Edits
data EditField
  = Eurl String
  | Etitle String
  | Edescription String
  | Etags String
  | Eprivate Boolean
  | Etoread Boolean

-- | Messages to parent
data BMessage
  = BNotifyRemove

type BSlot = H.Slot (Const Void) BMessage

type BState =
  { bm :: Bookmark
  , edit_bm :: Bookmark
  , deleteAsk:: Boolean
  , edit :: Boolean
  , loading :: Boolean
  , apiError :: Maybe String
  }

_bm :: Lens' BState Bookmark
_bm = lens _.bm (_ { bm = _ })

_edit_bm :: Lens' BState Bookmark
_edit_bm = lens _.edit_bm (_ { edit_bm = _ })

_edit :: Lens' BState Boolean
_edit = lens _.edit (_ { edit = _ })

_apiError :: Lens' BState (Maybe String)
_apiError = lens _.apiError (_ { apiError = _ })

_markdown = Proxy :: Proxy "markdown"

type ChildSlots =
  ( markdown :: Markdown.Slot Unit
  )

bmark :: forall q i. Bookmark -> H.Component q i BMessage Aff
bmark b' =
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
    , edit: false
    , loading: false
    , apiError: Nothing
    }

  render :: BState -> H.ComponentHTML BAction ChildSlots Aff
  render s@{ bm, edit_bm, apiError } =
    div [ id (show bm.bid) , class_ ("bookmark w-100 mw7 pa1 mb3" <> guard bm.private " private")] $
      [ whenH app.dat.isowner
          star
      , ifElseH s.edit
          display_edit
          display
      ]

    where

     star _ =
       div [ class_ ("star fl pointer" <> guard bm.selected " selected") ]
       [ button [ class_ "moon-gray", onClick \_ -> BStar (not bm.selected) ] [ text "✭" ] ]

     display _ =
        div [ class_ "display" ] $
        [ a [ href bm.url, target "_blank", class_ ("link f5 lh-title" <> guard bm.toread " unread")]
          [ text $ if S.null bm.title then "[no title]" else bm.title ]
        , br_
        , a [ href bm.url , class_ "link f7 gray hover-blue" ] [ text bm.url ]
        , a [ href (fromMaybe ("http://archive.is/" <> bm.url) (toMaybe bm.archiveUrl))
            , class_ ("link f7 gray hover-blue ml2" <> (guard (isJust (toMaybe bm.archiveUrl)) " green"))
            , target "_blank", title "archive link"]
            [ if isJust (toMaybe bm.archiveUrl) then text "☑" else text "☐" ]
        , br_
        , div [ class_ "description mt1 mid-gray" ] [ HH.slot _markdown unit Markdown.component bm.description absurd ]
        , div [ class_ "tags" ] $
              whenA (not (S.null bm.tags)) $ \_ ->
                map (\tag -> a [ class_ ("link tag mr1" <> guard (S.take 1 tag == ".") " private")
                             , href (linkToFilterTag tag) ]
                             [ text tag ])
                (S.split (Pattern " ") bm.tags)
              
        , a [ class_ "link f7 dib gray w4", href (linkToFilterSingle bm.slug), title shdatetime ]
          [ text shdate ]

        -- links
        , whenH app.dat.isowner $ \_ ->
            div [ class_ "edit_links di" ]
            [ button [ type_ ButtonButton, onClick \_ -> BEdit true, class_ "edit light-silver hover-blue" ] [ text "edit  " ]
            , div [ class_ "delete_link di" ]
              [ button [ type_ ButtonButton, onClick \_ -> BDeleteAsk true, class_ ("delete light-silver hover-blue" <> guard s.deleteAsk " dn") ] [ text "delete" ]
              , span ([ class_ ("confirm red" <> guard (not s.deleteAsk) " dn") ] )
                [ button [ type_ ButtonButton, onClick \_ -> BDeleteAsk false] [ text "cancel / " ]
                , button [ type_ ButtonButton, onClick \_ -> BDestroy, class_ "red" ] [ text "destroy" ]
                ] 
              ]
            ]
        , whenH app.dat.isowner $ \_ ->
            div [ class_ "read di" ] $
              guard bm.toread
              [ text "  "
              , button [ onClick \_ -> BMarkRead, class_ "mark_read" ] [ text "mark as read"]
              ]
        ]
       

     display_edit _ =
       div [ class_ "edit_bookmark_form pa2 pt0 bg-white" ] $
       [ whenH (isJust apiError)
              (alert_notification (fromMaybe "" apiError))
       , form [ onSubmit BEditSubmit ]
         [ div_ [ text "url" ]
         , input [ type_ InputUrl , class_ "url w-100 mb2 pt1 edit_form_input" , required true , name "url"
                 , value (edit_bm.url) , onValueChange (editField Eurl) ]
         , div_ [ text "title" ]
         , div [class_ "flex"]
               [input [ type_ InputText , class_ "title w-100 mb2 pt1 edit_form_input" , name "title"
                      , value (edit_bm.title) , onValueChange (editField Etitle) ]
               , button [ disabled s.loading, type_ ButtonButton, onClick \_ -> BLookupTitle, class_ ("ml1 pa1 mb2 dark-gray ba b--moon-gray bg-near-white pointer rdim f7 " <> guard s.loading "bg-light-silver") ] [ text "fetch" ]
               ]
         , div_ [ text "description" ]
         , textarea [ class_ "description w-100 mb1 pt1 edit_form_input" , name "description", rows 5
                    , value (edit_bm.description) , onValueChange (editField Edescription) ]
         , div [ id "tags_input_box"]
           [ div_ [ text "tags" ]
             , input [ id (tagid edit_bm), type_ InputText , class_ "tags w-100 mb1 pt1 edit_form_input" , name "tags"
                     , autocomplete AutocompleteOff, attr "autocapitalize" "off"
                     , value (edit_bm.tags) , onValueChange (editField Etags) ]
           ]
         , div [ class_ "edit_form_checkboxes mv3"]
           [ input [ type_ InputCheckbox , class_ "private pointer" , id "edit_private", name "private"
                   , checked (edit_bm.private) , onChecked (editField Eprivate) ]
           , text " "
           , label [ for "edit_private" , class_ "mr2" ] [ text "private" ]
           , text " "
           , input [ type_ InputCheckbox , class_ "toread pointer" , id "edit_toread", name "toread"
                   , checked (edit_bm.toread) , onChecked (editField Etoread) ]
           , text " "
           , label [ for "edit_toread" ] [ text "to-read" ]
           ]
         , input [ type_ InputSubmit , class_ "mr1 pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "save" ]
         , text " "
         , input [ type_ InputReset , class_ "pv1 ph2 dark-gray ba b--moon-gray bg-near-white pointer rdim" , value "cancel"
                 , onClick \_ -> BEdit false ]
         ]
       ]
       
     alert_notification alert_text _ = 
       div [ class_ "alert alert-err" ] [ text alert_text ]

     editField :: forall a. (a -> EditField) -> a -> BAction
     editField f = BEditField <<< f
     linkToFilterSingle slug = fromNullableStr app.userR <> "/b:" <> slug
     linkToFilterTag tag = fromNullableStr app.userR <> "/t:" <> encodeTag tag 
     shdate = toLocaleDateString bm.time 
     shdatetime = S.take 16 bm.time `append` "Z"

  tagid bm = show bm.bid <> "_tags"

  handleAction :: BAction -> H.HalogenM BState BAction ChildSlots BMessage Aff Unit

  -- | Star
  handleAction (BStar e) = do
    bm <- use _bm
    H.liftAff (toggleStar bm.bid (if e then Star else UnStar))
    _bm %= _ { selected = e }
    _edit_bm %= _ { selected = e }

  -- | Delete
  handleAction (BDeleteAsk e) = do
    H.modify_ (_ { deleteAsk = e })

  -- | Destroy
  handleAction (BDestroy) = do
    bm <- use _bm
    void $ H.liftAff (destroy bm.bid)
    H.raise BNotifyRemove

  -- | Mark Read
  handleAction (BMarkRead) = do
    bm <- use _bm
    void (H.liftAff (markRead bm.bid))
    _bm %= _ { toread = false }

  -- | Start/Stop Editing
  handleAction (BEdit e) = do
    bm <- use _bm
    _edit_bm .= bm
    _edit .= e
    _apiError .= Nothing
    H.liftEffect $
      when e
        (setFocus (tagid bm)) 

  -- | Update Form Field 
  handleAction (BEditField f) = do
    _edit_bm %= case f of
      Eurl e -> _ { url = e }
      Etitle e -> _ { title = e }
      Edescription e -> _ { description = e }
      Etags e -> _ { tags = e }
      Eprivate e -> _ { private = e }
      Etoread e -> _ { toread = e }

  -- | Lookup Title
  handleAction BLookupTitle = do
    H.modify_ (_ { loading = true })
    edit_bm <- H.gets _.edit_bm
    mtitle <- H.liftAff $ lookupTitle edit_bm
    case mtitle of
      Just title' -> _edit_bm %= (_ { title = title' })
      Nothing -> pure $ unit
    H.modify_ (_ { loading = false })

  -- | Submit
  handleAction (BEditSubmit e) = do
    H.liftEffect (preventDefault e)
    edit_bm <- use _edit_bm
    _apiError .= Nothing
    let edit_bm' = edit_bm { tags = S.replaceAll (Pattern ",") (Replacement " ") edit_bm.tags }
    H.liftAff (editBookmark edit_bm') >>= case _ of
      Left affErr -> do
        _apiError .= Just (printError affErr)
        liftEffect $ log (printError affErr)
      Right { status: StatusCode s } | s >= 200 && s < 300 -> do
        _bm .= edit_bm'
        _edit .= false
      Right res -> do
        _apiError .= Just (res.body)
        liftEffect $ log (res.body)
