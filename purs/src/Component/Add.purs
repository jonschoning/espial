module Component.Add where

import Prelude hiding (div)

import Affjax (printError)
import Affjax.StatusCode (StatusCode(..))
import App (destroy, editBookmark, lookupTitle, tagSuggestions)
import Autocomplete (SuggesterInstance, mkSuggester')
import Autocomplete.Types (Suggestions(..))
import Control.Bind (bindFlipped)
import Data.Array (last)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens (Lens', lens, use, (%=), (.=))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.String (Pattern(..), null, stripPrefix, trim)
import Data.String as S
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Globals (closeWindow, mmoment8601)
import Halogen as H
import Halogen.HTML (button, div, form, input, label, p, span, table, tbody_, td, td_, text, textarea, tr_)
import Halogen.HTML.Events (onChecked, onClick, onSubmit, onValueChange, onValueInput)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), autocomplete, autofocus, checked, disabled, for, id, name, required, rows, title, type_, value)
import Halogen.Subscription as HS
import Model (Bookmark, TSuggestion)
import Util (_curQuerystring, _loc, _doc, _lookupQueryStringValue, attr, class_, ifElseH, whenH)
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.HTMLDocument (referrer)
import Web.HTML.Location (setHref, origin)
import DOM.HTML.Indexed.AutocompleteType (AutocompleteType(..))

data BAction
  = BEditField EditField
  | BEditSubmit Event
  | BDeleteAsk Boolean
  | BLookupTitle
  | BDestroy
  | BInitialize
  | BTagsInput String
  | BTagsSuggestions (Suggestions TSuggestion)

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
  , loading :: Boolean
  , destroyed :: Boolean
  , apiError :: Maybe String
  , suggester :: Maybe (SuggesterInstance TSuggestion)
  , suggestions :: Suggestions TSuggestion
  }

_bm :: Lens' BState Bookmark
_bm = lens _.bm (_ { bm = _ })

_edit_bm :: Lens' BState Bookmark
_edit_bm = lens _.edit_bm (_ { edit_bm = _ })

_apiError :: Lens' BState (Maybe String)
_apiError = lens _.apiError (_ { apiError = _ })

addbmark :: forall q i o. Bookmark -> H.Component q i o Aff
addbmark b' =
  H.mkComponent
    { initialState: const (mkState b')
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just BInitialize
                                     }
    }
  where

  mkState b'' =
    let b = b'' { tags = "" }
    in
    { bm: b
    , edit_bm: b
    , deleteAsk: false
    , destroyed: false
    , loading: false
    , apiError: Nothing
    , suggester: Nothing
    , suggestions: Failed "No Results" []
    }

  render :: forall m. BState -> H.ComponentHTML BAction () m
  render s@{ bm, edit_bm, apiError } =
    ifElseH (not s.destroyed)
      display_edit
      display_destroyed
   where
     display_edit _ =
       form [ onSubmit BEditSubmit ]
       [ table [ class_ "w-100" ]
         [ tbody_
           [ tr_
             [ td [ class_ "w1" ] [ ]
             , td_ [ whenH (bm.bid > 0)
                       display_exists,
                     whenH (isJust apiError)
                       (alert_notification (fromMaybe "" apiError))
                   ]
             ]
           , tr_
             [ td_ [ label [ for "url" ] [ text "URL" ] ]
             , td_ [ input [ type_ InputUrl , id "url", class_ "w-100 mv1" , required true, name "url", autofocus (null bm.url)
                          , value (edit_bm.url) , onValueChange (editField Eurl)] ]
             ]
           , tr_
             [ td_ [ label [ for "title" ] [ text "title" ] ]
             , td [class_ "flex"]
                  [ input [ type_ InputText , id "title", class_ "w-100 mv1 flex-auto" , name "title" , value (edit_bm.title) , onValueChange (editField Etitle)]
                  , button [ disabled s.loading, type_ ButtonButton, onClick \_ -> BLookupTitle, class_ ("ml2 input-reset ba b--navy pointer f6 di dim pa1 ma1 mr0 " <> guard s.loading "bg-light-silver") ] [ text "fetch" ]
                  ]
             ]
           , tr_
             [ td_ [ label [ for "description" ] [ text "description" ] ]
             , td_ [ textarea [ class_ "w-100 mt1 mid-gray" , id "description", name "description", rows 4
                              , value (edit_bm.description) , onValueChange (editField Edescription)] ]
             ]
           , tr_
             [ td_ [ label [ for "tags" ] [ text "tags" ] ]
             , td_ [ input [ type_ InputText , id "tags", class_ "w-100 mv1" , name "tags", autocomplete AutocompleteOff, attr "autocapitalize" "off", autofocus (not $ null bm.url)
                           , value (edit_bm.tags) , onValueInput BTagsInput, onValueChange (editField Etags)] ]
             ]
           , tr_
             [ td_ [ label [ for "private" ] [ text "private" ] ]
             , td_ [ input [ type_ InputCheckbox , id "private", class_ "private pointer" , name "private"
                           , checked (edit_bm.private) , onChecked (editField Eprivate)] ]
             ]
           , tr_
             [ td_ [ label [ for "toread" ] [ text "read later" ] ]
             , td_ [ input [ type_ InputCheckbox , id "toread", class_ "toread pointer" , name "toread"
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

     display_exists _ = 
       div [ class_ "alert" ]
       [ text "previously saved "
       , span [ class_ "link f7 dib gray pr3" , title (maybe bm.time snd mmoment) ]
         [ text (maybe " " fst mmoment) ]
       , div [ class_ "edit_links dib ml1" ]
         [ div [ class_ "delete_link di" ]
           [ button ([ type_ ButtonButton, onClick \_ -> BDeleteAsk true, class_ "delete" ] <> guard s.deleteAsk [ attr "hidden" "hidden" ]) [ text "delete" ]
           , span ([ class_ "confirm red" ] <> guard (not s.deleteAsk) [ attr "hidden" "hidden" ])
             [ button [ type_ ButtonButton, onClick \_ -> BDeleteAsk false] [ text "cancel / " ]
             , button [ type_ ButtonButton, onClick \_ -> BDestroy, class_ "red" ] [ text "destroy" ]
             ] 
           ]
         ]
       ]

     alert_notification alert_text _ = 
       div [ class_ "alert alert-err" ] [ text alert_text ]

     display_destroyed _ = p [ class_ "red"] [text "you killed this bookmark"]

     editField :: forall a. (a -> EditField) -> a -> BAction
     editField f = BEditField <<< f
     mmoment = mmoment8601 bm.time

  handleAction :: BAction -> H.HalogenM BState BAction () o Aff Unit
  handleAction BInitialize = do
    suggester <- liftEffect $ mkSuggester'
        { fetch: fetchFn
        , inputDebounce: Milliseconds 500.0
        , inputTransformer: identity
        }
    H.modify_ (_ {suggester = Just suggester})
    { emitter, listener } <- liftEffect HS.create
    void $ H.subscribe emitter
    liftEffect $ suggester.subscribe (HS.notify listener <<< BTagsSuggestions)
    where
      fetchFn :: String -> Aff (Either String (Array TSuggestion))
      fetchFn query =
         tagSuggestions { query: query, suggestions: [] }
         <#> bindFlipped (\s -> if A.null s.suggestions then Left "No Results" else Right s.suggestions)

  handleAction (BDeleteAsk e) = do
    H.modify_ (_ { deleteAsk = e })

  handleAction BLookupTitle = do
    H.modify_ (_ { loading = true })
    edit_bm <- H.gets _.edit_bm
    mtitle <- H.liftAff $ lookupTitle edit_bm
    case mtitle of
      Just title' -> _edit_bm %= (_ { title = title' })
      Nothing -> pure $ unit
    H.modify_ (_ { loading = false })

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

  handleAction (BTagsInput e) = do
    edit_bm_tags <- H.gets _.edit_bm.tags
    let edit_bm_tags_last = getLast edit_bm_tags
        e_last = getLast e
        e_send = if (S.length e_last >= 2 && edit_bm_tags_last /= e_last)
                   then e_last
                    else ""
    H.gets _.suggester >>= traverse_ \suggester ->
      liftEffect $ suggester.send e_send
    handleAction (BEditField (Etags e))
    where
      getLast = fromMaybe "" <<< last <<< S.split (Pattern " ") <<< trim

  handleAction (BTagsSuggestions e) = do
    liftEffect $ log ("suggestions:  " <> show e)
    H.modify_ (_ { suggestions = e })

  handleAction (BEditSubmit e) = do
    liftEffect (preventDefault e)
    edit_bm <- use _edit_bm 
    _apiError .= Nothing
    H.liftAff (editBookmark edit_bm) >>= case _ of
      Left affErr -> do
        _apiError .= Just (printError affErr)
        liftEffect $ log (printError affErr)
      Right { status: StatusCode s } | s >= 200 && s < 300 -> do
        _bm .= edit_bm
        qs <- liftEffect $ _curQuerystring
        doc <- liftEffect $ _doc
        ref <- liftEffect $ referrer doc
        loc <- liftEffect $ _loc
        org <- liftEffect $ origin loc
        case _lookupQueryStringValue qs "next" of
          Just "closeWindow" -> liftEffect $ closeWindow =<< window
          Just "back" -> liftEffect $
            case stripPrefix (Pattern org) ref of
              Just _ -> setHref ref loc
              Nothing -> setHref org loc
          _ -> liftEffect $ closeWindow =<< window
      Right res -> do
        _apiError .= Just (res.body)
        liftEffect $ log (res.body)
