module Component.Markdown (component) where

import Component.RawHtml as RH
import Effect.Aff (Aff)
import Foreign.Marked (marked)
import Halogen as H
import Halogen.HTML as HH

component :: forall q o. H.Component HH.HTML q String o Aff
component = RH.mkComponent marked
