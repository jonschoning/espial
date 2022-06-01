module Component.TagCloud where

import Prelude hiding (div)

import App (getTagCloud, updateTagCloudMode)
import Data.Array (concat, cons, delete, notElem, null, sortBy)
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.String (joinWith, toLower, null) as S
import Data.String (toLower)
import Data.Tuple (fst, uncurry)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable, empty, values) as F
import Globals (app')
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML (HTML, a, attr, button, div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), href, title, type_)
import Data.Number (log)
import Model (TagCloud, TagCloudModeF(..), isExpanded, isRelated, setExpanded, tagCloudModeFromF)
import Util (class_, encodeTag, fromNullableStr, ifElseA, whenH)

data TAction
  = TInitialize
  | TExpanded Boolean
  | TChangeMode TagCloudModeF

type TState =
  { mode :: TagCloudModeF
  , tagcloud :: TagCloud
  }

_mode :: Lens' TState TagCloudModeF
_mode = lens _.mode (_ { mode = _ })

tagcloudcomponent :: forall q i o. TagCloudModeF -> H.Component q i o Aff
tagcloudcomponent m' =
  H.mkComponent
    { initialState: const (mkState m')
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just TInitialize
                                     }
    }
  where
  app = app' unit
  mkState m =
    { mode: m
    , tagcloud: F.empty
    }

  render :: TState -> H.ComponentHTML TAction () Aff
  render { mode:TagCloudModeNone } =
    div [class_ "tag_cloud" ] []
  render { mode, tagcloud } =
    div [class_ "tag_cloud mv3" ] 
    [
      div [class_ "tag_cloud_header mb2"] $
          ifElseA (isRelated mode)
            (\_ -> do --RELATED
              [ button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue mr1 b")
                       , onClick \_ -> TExpanded (not (isExpanded mode))
                       ] [text "Related Tags"]
              ]
            ) 
            (\_ -> do -- NOT RELATED
              [ button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue mr1" <> guard (mode == modetop) " b")
                       , title "show a cloud of your most-used tags"
                       , onClick \_ -> TChangeMode modetop
                       ] [text "Top Tags"]
              , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue ml2 " <> guard (mode == modelb1) " b") 
                       , title "show all tags"
                       , onClick \_ -> TChangeMode modelb1
                       ] [text "all"] 
              , text "‧"
              , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb2) " b") 
                       , title "show tags with at least 2 bookmarks"
                       , onClick \_ -> TChangeMode modelb2
                       ] [text "2"]
              , text "‧"
              , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb5) " b") 
                       , title "show tags with at least 5 bookmarks"
                       , onClick \_ -> TChangeMode modelb5
                       ] [text "5"]
              , text "‧"
              , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb10) " b") 
                       , title "show tags with at least 10 bookmarks"
                       , onClick \_ -> TChangeMode modelb10
                       ] [text "10"]
              , text "‧"
              , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb20) " b") 
                       , title "show tags with at least 20 bookmarks"
                       , onClick \_ -> TChangeMode modelb20
                       ] [text "20"]
              ])
              <> [button [ type_ ButtonButton, class_ "pa1 ml2 f7 link silver hover-blue "
                         , onClick \_ -> TExpanded (not (isExpanded mode))]
                         [ text (if isExpanded mode then "hide" else "show") ]]
    , whenH (isExpanded mode) \_ -> do
        let n = fromMaybe 1 (minimum (F.values tagcloud))
            m = fromMaybe 1 (maximum (F.values tagcloud))
        div [class_ "tag_cloud_body"] $ case mode of
          TagCloudModeNone -> []
          (TagCloudModeRelated _ curtags) -> 
            toArray curtags n m tagcloud
          _ -> 
            toArray [] n m tagcloud
          
    ]
    where
      modetop = TagCloudModeTop (isExpanded mode) 200
      modelb1 = TagCloudModeLowerBound (isExpanded mode) 1
      modelb2 = TagCloudModeLowerBound (isExpanded mode) 2
      modelb5 = TagCloudModeLowerBound (isExpanded mode) 5
      modelb10 = TagCloudModeLowerBound (isExpanded mode) 10
      modelb20 = TagCloudModeLowerBound (isExpanded mode) 20


  toArray :: Array String -> Int -> Int -> Object Int -> Array (HTML _ _)
  toArray curtags n m =
    concat
    <<< map (uncurry (toSizedTag (map toLower curtags) n m))
    <<< sortBy (comparing (S.toLower <<< fst))
    <<< F.toUnfoldable

  linkToFilterTag rest = fromNullableStr app.userR <> (if S.null rest then "" else "/t:" <> rest) 

  toSizedTag :: Array String -> Int -> Int -> String -> Int -> _
  toSizedTag curtags n m k v =
    [ a [ href (linkToFilterTag (encodeTag k)), class_ "link tag mr1" , style] 
        [ text k ]
      , whenH (not (null curtags))  \_ -> if (notElem k_lower curtags)
          then a [href (linkToFilterTag (S.joinWith "+" (map encodeTag (cons k_lower curtags)))), class_ "link mr2 tag-include"] [text "⊕"]
          else a [href (linkToFilterTag (S.joinWith "+" (map encodeTag (delete k_lower curtags)))), class_ "link mr2 tag-exclude"] [text "⊖"]
    ]
    where
      k_lower = toLower k
      fontsize = rescale identity (toNumber v) (toNumber n) (toNumber m) 100.0 150.0
      opacity = rescale (log <<< (1.0 + _)) (toNumber v) (toNumber n) (toNumber m) 0.6 1.0
      style = attr (AttrName "style") ("font-size:" <> show fontsize <> "%" <> ";opacity:" <> show opacity)

  rescale :: (Number -> Number) -> Number -> Number -> Number -> Number -> Number -> Number
  rescale f v n m l h = (if m - n < 0.01 then 1.0 else (f (v - n) / f (m - n))) * (h - l) + l

  fetchTagCloud :: TagCloudModeF -> H.HalogenM TState TAction () o Aff Unit
  fetchTagCloud mode' = do
    case mode' of
      TagCloudModeNone -> pure unit
      _ -> do
        tagcloud <- H.liftAff $ getTagCloud (tagCloudModeFromF mode')
        H.modify_ (\s -> s { 
          mode = mode',
          tagcloud = fromMaybe F.empty tagcloud
        })
  
  handleAction :: TAction -> H.HalogenM TState TAction () o Aff Unit
  handleAction TInitialize = do
    mode <- H.gets _.mode
    fetchTagCloud mode
  handleAction (TExpanded expanded) = do
    H.modify_ (\s -> s { mode = setExpanded s.mode expanded })
    mode <- H.gets _.mode
    void $ H.liftAff $ updateTagCloudMode (tagCloudModeFromF mode)
  handleAction (TChangeMode mode') = do
    mode <- H.gets _.mode
    if mode == mode'
       then handleAction (TExpanded (not (isExpanded mode)))
       else fetchTagCloud (setExpanded mode' true)
