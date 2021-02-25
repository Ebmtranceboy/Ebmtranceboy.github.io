module Main where

import Prelude
import CSS (Float(..), a, backgroundColor, black, color, float, fontSize, fromHexString, fromString, hover, nav, noneTextDecoration, padding, px, textDecoration, white, with, (?))       
import CSS.TextAlign (textAlign, center)
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.CSS as HC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource (Finalizer(..), affEventSource, emit) as EventSource
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Properties as HP
import Simple.JSON (E, readJSON, writeJSON)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window) as DOM
import Web.HTML.Window (localStorage) as DOM
import Web.Storage.Storage (getItem, setItem) as Storage
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key) as Event

type Note =
  { text ∷ String
  }

type State = 
  { name ∷ String 
  , notes ∷ Array Note
  , elapsed :: Int
  }

data Action 
  = Initialize
  | UpdateName String
  | AddTodo
  | DeleteAll Int
  | Tick

pageRender ∷ forall m. MonadAff m => H.Component HH.HTML (Const Void) Int Void m 
pageRender = 
  H.mkComponent 
    { initialState
    , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction 
      }
    , render
    }

pageStyle ∷ forall m. MonadAff m => H.Component HH.HTML (Const Void) Int Void m 
pageStyle = 
  H.mkComponent 
    { initialState
    , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction 
      }
    , render: styleComponent
    }

initialState ∷ Int -> State
initialState n = 
  { name: "" 
  , notes: []
  , elapsed: n
  }

update :: State -> State
update state = 
  if state.name == ""
    then state
    else state { name = ""
               , notes = Array.snoc state.notes {text: state.name} 
               }

storageKey ∷ String
storageKey = "notes"

toStorage :: State -> Effect Unit
toStorage state =
  DOM.window
      >>= DOM.localStorage
      >>= Storage.setItem storageKey (writeJSON { notes: state.notes })

fromStorage :: Effect State
fromStorage = do
  storedModel ←
    DOM.window
      >>= DOM.localStorage
      >>= Storage.getItem storageKey
      >>> map (_ >>= (readJSON :: String -> E { notes :: Array Note }) >>> hush)
  pure $ case storedModel of
      Nothing → initialState 0
      Just sm → (initialState 0)
        { notes = sm.notes
        }

timer :: forall m. MonadAff m => EventSource m Action
timer = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    EventSource.emit emitter Tick

  pure $ EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber
  
handleAction :: forall m
  .  MonadAff m 
  => Action -> H.HalogenM State Action () Void m Unit
handleAction ( Initialize ) = do
  _ <- H.subscribe timer
  H.liftEffect fromStorage >>= H.put
  
handleAction ( UpdateName newName ) = 
  H.modify_ _{ name = newName }

handleAction ( AddTodo ) = do
  H.modify_ update
  H.get >>= (toStorage >>> H.liftEffect) 

handleAction ( DeleteAll n) = do
  H.modify_ $ const $ initialState n
  H.liftEffect $ toStorage $ initialState n
  
handleAction ( Tick ) =
  H.modify_ \state -> state { elapsed = state.elapsed + 1 }

onEnter ∷ forall i r. i → HH.IProp (onKeyDown ∷ Event.KeyboardEvent | r) i
onEnter a = HE.onKeyDown \ev →
  if Event.key ev == "Enter"
    then Just a
    else Nothing

composeNote :: forall m. State -> HH.HTML m Action
composeNote state =
    HH.div_ [ HH.p_ [ HH.text $ "Started " <> show state.elapsed <> " seconds ago." ]
            , HH.p_ [ HH.text "What is your name?" ]
            , HH.input [ HP.type_ HP.InputText
                       , HP.value state.name
                       , HP.placeholder "type note"
                       , HP.autofocus true 
                       , HE.onValueInput $ Just <<< UpdateName
                       , onEnter AddTodo
                       ]
            , hello
            ]

    where
      hello = if state.name == ""
              then HH.p_ []
              else HH.p_ [ HH.text $ "Hello " <> state.name ]

displayNotes :: forall m. State -> HH.HTML m Action
displayNotes state =
    HH.div_ [ HH.nav_ $
              Array.mapWithIndex (\i note -> 
                HH.a 
                  [HP.href $ "#note" <> show i
                  ] 
                  [ HH.label_ [HH.text note.text] ]
                ) 
                 state.notes
            ]
 
render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HH.div_ [ composeNote state
            , displayNotes state
            , HH.button 
              [HE.onClick $ const $ Just $ DeleteAll state.elapsed] 
              [HH.text $ "Clear All"]
            ]
styleComponent :: forall m t. State -> HH.HTML m t
styleComponent _ = 
  HC.stylesheet $ do
    nav ? do
      a ? do
        backgroundColor black
        float FloatLeft 
        color $ fromMaybe black $ fromHexString "#f2f2f2"
        textAlign center
        padding (px 14.0) (px 16.0) (px 14.0) (px 16.0) 
        textDecoration noneTextDecoration
        fontSize $ px 17.0
        
      a `with` hover ? do
        backgroundColor $ fromMaybe black $ fromHexString "#ddd"
        color black
      a `with` fromString ":active" ? do
        backgroundColor $ fromMaybe black $ fromHexString "#4CAF50"
        color white
      
main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  let drivenBy tag component = 
        traverse_ (runUI component 0) =<< HA.selectElement (QuerySelector tag)
  "head" `drivenBy` pageStyle
  "body" `drivenBy` pageRender
