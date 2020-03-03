module Mecanism.Graph where

import Prelude

import Data.Array ((!!)) as Array
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

nth :: forall a. Array a -> Int -> a
nth xs i =  unsafePartial fromJust $ xs Array.!! i

infixl 6 nth as !!
