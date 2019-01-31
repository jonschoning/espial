module Foreign.Marked where

import Prelude
import Globals (RawHTML(..))

foreign import markedImpl :: String -> String

marked :: String -> RawHTML
marked = RawHTML <<< markedImpl
