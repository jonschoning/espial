module Component.RawHtml where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, makeAff)
import Effect.Class (liftEffect)
import Globals (RawHTML(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

foreign import unsafeSetInnerHTML :: HTMLElement -> RawHTML -> Effect Unit

data Action i
  = SetInnerHTML
  | Receive (Input i)

type Input :: forall k. k -> k
type Input i = i

type State i =
  { elRef :: H.RefLabel
  , inputval :: Input i
  }

component :: forall q o. H.Component q (Input String) o Aff
component = mkComponent RawHTML

mkComponent :: forall q i o. (Input i -> RawHTML) -> H.Component q (Input i) o Aff
mkComponent toRawHTML =
  H.mkComponent
    { initialState: \inputval -> { elRef: H.RefLabel "inputval", inputval } 
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction
                                    , initialize = Just SetInnerHTML
                                    , receive = Just <<< Receive
                                    })
    }
  where
  render :: forall m. (State i) -> H.ComponentHTML (Action i) () m
  render state = 
    HH.div 
      [ HP.ref state.elRef ] 
      []

  handleAction :: (Action i) -> H.HalogenM (State i) (Action i) () o Aff Unit
  handleAction = case _ of
    SetInnerHTML -> do
      { elRef } <- H.get
      mel <- H.getHTMLElementRef elRef
      for_ mel \el -> do  
        { inputval } <- H.get
        H.liftAff $ forkAff $ makeAff \_ -> do
          liftEffect $ unsafeSetInnerHTML el (toRawHTML inputval)
          mempty
      pure unit
    
    Receive inputval -> do
      H.modify_ _ { inputval = inputval }
      handleAction $ SetInnerHTML
