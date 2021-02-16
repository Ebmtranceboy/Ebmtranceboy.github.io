module Hooks.UseLocalStorage
  ( useLocalStorage
  , UseLocalStorage
  , Key(..)
  , StorageInterface
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Hooks.UseInitializer (UseInitializer, useInitializer)
import Halogen.Hooks (Hook, HookM, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype UseLocalStorage a hooks =
  UseLocalStorage (UseEffect (UseInitializer (UseState a hooks)))

derive instance newtypeUseLocalStorage :: Newtype (UseLocalStorage a hooks) _

type StorageInterface a =
  { key :: Key
  , defaultValue :: a
  , toJson :: a -> String
  , fromJson :: String -> a
  }

-- | A key for a cell in local storage
newtype Key = Key String

derive newtype instance eqKey :: Eq Key

useLocalStorage
  :: forall m a
   . MonadEffect m
  => Eq a
  => StorageInterface a
  -> Hook m (UseLocalStorage a) (a /\ ((a -> a) -> HookM m Unit))
useLocalStorage { key, defaultValue, toJson, fromJson } = Hooks.wrap Hooks.do
  state /\ stateId <- Hooks.useState defaultValue

  let Key k = key

  useInitializer do
    storage <- liftEffect (localStorage =<< window)
    mbItem <- liftEffect (getItem k storage)
    mbItem # maybe
      (liftEffect $ setItem k (toJson defaultValue) storage)
      (\item -> Hooks.modify_ stateId \_ -> fromJson item)

  useWriteStorage { value: state, key: k }

  Hooks.pure (Tuple state (Hooks.modify_ stateId))
  where
  useWriteStorage deps = Hooks.captures deps Hooks.useTickEffect do
    liftEffect do
      storage <- localStorage =<< window
      setItem deps.key (toJson deps.value) storage
    pure Nothing
