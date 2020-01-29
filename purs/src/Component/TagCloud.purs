module Component.TagCloud where

import Prelude hiding (div)

import App (getTagCloud, updateTagCloudMode)
import Data.Array (sortBy)
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.String (toLower) as S
import Data.Tuple (fst, uncurry)
import Effect.Aff (Aff)
import Foreign.Object (toUnfoldable, empty, values) as F
import Globals (app')
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML (HTML, a, attr, button, div, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ButtonType(..), href, title, type_)
import Math (log)
import Model (TagCloud, TagCloudModeF(..), isExpanded, setExpanded, tagCloudModeFromF)
import Util (class_, fromNullableStr, whenH)

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

tagcloudcomponent :: forall q i o. TagCloudModeF -> H.Component HTML q i o Aff
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
  render s@{ mode:TagCloudModeNone } =
    div [class_ "tag_cloud" ] []
  render s@{ mode, tagcloud } =
    div [class_ "tag_cloud mv3" ] 
    [ div [class_ "tag_cloud_header mb2"]
      [ button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue mr1" <> guard (mode == modetop) " b")
               , title "show a cloud of your most-used tags"
               , onClick \_ -> Just (TChangeMode modetop)
               ] [text "Top Tags"]
      , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue ml2 " <> guard (mode == modelb1) " b") 
               , title "show all tags"
               , onClick \_ -> Just (TChangeMode modelb1)
               ] [text "all"] 
      , text "‧"
      , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb2) " b") 
               , title "show tags with at least 2 bookmarks"
               , onClick \_ -> Just (TChangeMode modelb2)
               ] [text "2"]
      , text "‧"
      , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb5) " b") 
               , title "show tags with at least 5 bookmarks"
               , onClick \_ -> Just (TChangeMode modelb5)
               ] [text "5"]
      , text "‧"
      , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb10) " b") 
               , title "show tags with at least 10 bookmarks"
               , onClick \_ -> Just (TChangeMode modelb10)
               ] [text "10"]
      , text "‧"
      , button [ type_ ButtonButton, class_ ("pa1 f7 link hover-blue" <> guard (mode == modelb20) " b") 
               , title "show tags with at least 20 bookmarks"
               , onClick \_ -> Just (TChangeMode modelb20)
               ] [text "20"]
      , button [ type_ ButtonButton, class_ "pa1 ml2 f7 link silver hover-blue "
               , onClick \_ -> Just (TExpanded (not (isExpanded mode)))]
               [ text (if isExpanded mode then "hide" else "show") ]
      ]
    , whenH (isExpanded mode) $ \_ -> do
        let n = fromMaybe 1 (minimum (F.values tagcloud))
            m = fromMaybe 1 (maximum (F.values tagcloud))
        div [class_ "tag_cloud_body"] $ case mode of
          TagCloudModeNone -> []
          _ -> toArray n m tagcloud
          
    ]
    where
      modetop = TagCloudModeTop (isExpanded mode) 200
      modelb1 = TagCloudModeLowerBound (isExpanded mode) 1
      modelb2 = TagCloudModeLowerBound (isExpanded mode) 2
      modelb5 = TagCloudModeLowerBound (isExpanded mode) 5
      modelb10 = TagCloudModeLowerBound (isExpanded mode) 10
      modelb20 = TagCloudModeLowerBound (isExpanded mode) 20


  toArray :: Int -> Int -> _
  toArray n m =
    map (uncurry (toSizedTag n m))
    <<< sortBy (comparing (S.toLower <<< fst))
    <<< F.toUnfoldable

  linkToFilterTag tag = fromNullableStr app.userR <> "/t:" <> tag
  toSizedTag :: Int -> Int -> String -> Int -> _
  toSizedTag n m k v =
    a [ href (linkToFilterTag k)
      , class_ "link tag mr1"
      , attr (AttrName "style") ("font-size:" <> show fontsize <> "%" <>
                                 ";opacity:" <> show opacity)
      ] [text k]
    where
      fontsize = rescale identity (toNumber v) (toNumber n) (toNumber m) 100.0 150.0
      opacity = rescale (log <<< (1.0 + _)) (toNumber v) (toNumber n) (toNumber m) 0.6 1.0

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
