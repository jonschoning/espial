module Component.Markdown (component, MInput, MQuery, MOutput, module RHExt) where

import Component.RawHtml as RH
import Component.RawHtml (Query(Receive)) as RHExt
import Effect.Aff (Aff)
import Foreign.Marked (marked)
import Halogen as H
import Halogen.HTML as HH

type MInput = String
type MQuery = RH.Query String
type MOutput = RH.Output

component :: H.Component HH.HTML MQuery MInput MOutput Aff
component = RH.mkComponent marked
