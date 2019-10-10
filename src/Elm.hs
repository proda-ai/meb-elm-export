{-| Generate Elm types, JSON decoders & JSON encoders from Haskell datatypes.
-}
module Elm
  ( module X
  ) where

import Elm.Common as X (Options(..), defaultOptions, require)
import Elm.Decoder as X
import Elm.StringTo as X
import Elm.Encoder as X
import Elm.StringFrom as X
import Elm.File as X
import Elm.Fuzz as X
import Elm.Dummy as X
import Elm.Record as X
import Elm.Type as X
