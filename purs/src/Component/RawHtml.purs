module Component.RawHtml where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Globals (RawHTML(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

foreign import unsafeSetInnerHTML :: HTMLElement -> RawHTML -> Effect Unit

data Query i a
  = SetInnerHTML a
  | Receive (Input i) a

type Input i = i

type Output = Void

type State i =
  { elRef :: H.RefLabel
  , inputval :: Input i
  }

component :: H.Component HH.HTML (Query String) (Input String) Output Aff
component = mkComponent RawHTML

mkComponent :: forall i. (Input i -> RawHTML) -> H.Component HH.HTML (Query i) (Input i) Output Aff
mkComponent toRawHTML = H.lifecycleComponent
  { initialState: \inputval -> { elRef: H.RefLabel "inputval", inputval } 
  , render
  , eval
  , receiver: HE.input Receive
  , initializer: Just $ H.action SetInnerHTML
  , finalizer: Nothing
  }
  where
  render :: (State i) -> H.ComponentHTML (Query i)
  render state = 
    HH.div 
      [ HP.ref state.elRef ] 
      []

  eval :: (Query i) ~> H.ComponentDSL (State i) (Query i) Output Aff
  eval = case _ of
    SetInnerHTML a -> do
      { elRef } <- H.get
      mel <- H.getHTMLElementRef elRef
      for_ mel \el -> do  
        { inputval } <- H.get
        H.liftEffect (unsafeSetInnerHTML el (toRawHTML inputval))
      pure a
    
    Receive inputval a -> do
      H.modify_ _ { inputval = inputval }
      eval $ SetInnerHTML a
