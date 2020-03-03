module Mecanism  where

import Mecanism.Graph (nth)
import Mecanism.Types (System) as Types

type System a = Types.System a

infixl 6 nth as !!
