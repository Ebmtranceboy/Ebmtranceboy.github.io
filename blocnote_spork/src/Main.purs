module Main where

import Prelude

import Data.Array (snoc) as Array
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Simple.JSON (readJSON, writeJSON)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (liftNat, merge, never)
import Web.HTML (window) as DOM
import Web.HTML.Window (localStorage) as DOM
import Web.Storage.Storage (getItem, setItem) as Storage
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key) as Event

type Model =
  { notes ∷ Array Note
  , pending ∷ String
  }

type Note =
  { text ∷ String
  }

initialModel ∷ Model
initialModel =
  { notes: []
  , pending: ""
  }

data Action
  = None
  | UpdatePending String
  | AddTodo
  | DeleteAll

toStorage ∷ Model → App.Transition StorageEffect Model Action
toStorage model =
  { model
  , effects: App.lift (WriteStorage model None)
  }

update ∷ Model → Action → App.Transition StorageEffect Model Action
update model = case _ of
  None →
    App.purely model

  UpdatePending pending →
    App.purely $ model { pending = pending }

  AddTodo →
    let
      notes' =
        if model.pending == ""
          then model.notes
          else Array.snoc model.notes {text: model.pending}
    in
      toStorage $ model
        { pending = ""
        , notes = notes'
        }

  DeleteAll -> toStorage model{pending = "", notes = []}

onEnter ∷ forall i r. i → H.IProp (onKeyDown ∷ Event.KeyboardEvent | r) i
onEnter a = H.onKeyDown \ev →
  if Event.key ev == "Enter"
    then Just a
    else Nothing

render ∷ Model → H.Html Action
render model =
  H.div []
    [ H.input
          [ H.type_ H.InputText
          , H.value model.pending
          , H.placeholder "type note"
          , H.autofocus true
          , H.onValueInput (H.always UpdatePending)
          , onEnter AddTodo
          ]
    , H.ul [] $
      map (\note -> H.li [] [H.label [] [H.text note.text]]) model.notes

    , H.button [H.onClick (H.always_ DeleteAll)] [H.text $ "Clear All"]
    ]

app ∷ Maybe StoredModel → App.App StorageEffect (Const Void) Model Action
app storedModel =
  { render
  , update
  , subs: const mempty
  , init: App.purely model
  }
  where
  model = case storedModel of
    Nothing → initialModel
    Just sm → initialModel
      { notes = sm.notes
      }

type StoredModel =
  { notes ∷ Array Note
  }

storageKey ∷ String
storageKey = "notes"

data StorageEffect a
  = WriteStorage Model a

derive instance functorStorageEffect ∷ Functor StorageEffect

runTodoEffect ∷ StorageEffect ~> Effect
runTodoEffect = case _ of
  WriteStorage model next → do
    let
      storedModel =
        { notes: model.notes
        }
    DOM.window
      >>= DOM.localStorage
      >>= Storage.setItem storageKey (writeJSON storedModel)
    pure next

main ∷ Effect Unit
main = do
  storedModel ←
    DOM.window
      >>= DOM.localStorage
      >>= Storage.getItem storageKey
      >>> map (_ >>= readJSON >>> hush)

  inst ←
    App.makeWithSelector
      (liftNat runTodoEffect `merge` never)
      (app storedModel)
      "#app"
  inst.run
  

