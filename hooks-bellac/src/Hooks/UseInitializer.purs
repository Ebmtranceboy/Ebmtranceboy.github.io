module Hooks.UseInitializer
  ( useInitializer
  , UseInitializer
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.Hooks (HookM, Hook, UseEffect)
import Halogen.Hooks as Hooks

type UseInitializer' = UseEffect
foreign import data UseInitializer :: Hooks.HookType

instance newtypeUseInitializer :: Hooks.HookNewtype UseInitializer UseInitializer'

useInitializer :: forall m. HookM m Unit -> Hook m UseInitializer Unit
useInitializer initializer = Hooks.wrap do
  Hooks.useLifecycleEffect (initializer *> pure Nothing)
  
