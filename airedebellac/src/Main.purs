module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Const (Const)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime (..), Time (..))
import Data.Enum (toEnum)
import Data.Either (hush)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.String (Pattern (..), split)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Global (readFloat)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource (Finalizer(..), affEventSource, emit) as EventSource
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Properties as HP
import Simple.JSON (E, readJSON, writeJSON)
import Web.HTML (window) as DOM
import Web.HTML.Window (localStorage) as DOM
import Web.Storage.Storage (getItem, setItem) as Storage
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key) as Event

type Note =
  { text ∷ String
  }
  
type DateValues = { year :: Int, month :: Int, day :: Int }

type State = 
  { name ∷ String 
  , notes ∷ Array Note
  , elapsed :: Int
  , baseDate :: Maybe DateValues
  , emplacement :: Int
  }

data Action 
  = Initialize
  | UpdateName String
  | UpdateDate String
  | UpdateEmplacement String
  | AddTodo
  | DeleteAll Int
  | Tick

page ∷ forall m. MonadAff m => H.Component HH.HTML (Const Void) Int Void m 
page = 
  H.mkComponent 
    { initialState
    , eval: H.mkEval $ H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction 
      }
    , render
    }

initialState ∷ Int -> State
initialState n = 
  { name: "" 
  , notes: []
  , elapsed: n
  , baseDate: Nothing
  , emplacement: 1
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
      >>= Storage.setItem storageKey (writeJSON { notes: state.notes, baseDate: state.baseDate })

fromStorage :: Effect State
fromStorage = do
  storedModel ←
    DOM.window
      >>= DOM.localStorage
      >>= Storage.getItem storageKey
      >>> map (_ >>= (readJSON :: String -> E { notes :: Array Note, baseDate :: Maybe DateValues }) >>> hush)
  pure $ case storedModel of
      Nothing → initialState 0
      Just sm → (initialState 0)
        { notes = sm.notes
        , baseDate = sm.baseDate
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

handleAction ( UpdateEmplacement newEmpl ) = 
  H.modify_ _{ emplacement = round $ readFloat newEmpl }

handleAction ( UpdateDate iAmADate ) = do
  H.modify_ _{ baseDate = prove iAmADate }
  H.get >>= (toStorage >>> H.liftEffect) 

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

today :: Maybe Date
today = canonicalDate <$> toEnum 2021 <*> toEnum 2 <*> toEnum 1

valuesToDate :: Maybe DateValues -> Maybe DateTime
valuesToDate mvs = mvs >>= \{year, month, day} ->  
   DateTime <$> (canonicalDate <$> toEnum year <*> toEnum month <*> toEnum day) 
            <*> (Time <$> toEnum 8 <*> toEnum 30 <*> toEnum 0 <*> toEnum 0)

now :: Maybe DateTime
now = DateTime <$> today <*> (Time <$> toEnum 8 <*> toEnum 30 <*> toEnum 0 <*> toEnum 0)

prove :: String -> Maybe DateValues
prove str = 
  let ns = split (Pattern "-") str
      int = round <<< readFloat
  in (\ a b c -> {year: int a, month: int b, day: int c}) 
    <$> (ns Array.!! 0) <*> (ns Array.!! 1) <*> (ns Array.!! 2)

render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HH.div_ [ HH.p_ [ HH.text $ "Started " <> show state.elapsed <> " seconds ago." ]
            , HH.p_ [ HH.text $ "Today : " <> show today ]
            , HH.p_ [ HH.text $ "Base : " <> show (valuesToDate state.baseDate) ]
            , HH.input [ HP.type_ HP.InputDate
                       , HE.onValueInput $ Just <<< UpdateDate
                       ]
            , HH.input [ HP.type_ HP.InputRadio
                       , HP.name "emplacements"
                       , HP.id_ "empl1"
                       , HP.value "1"
                       , HE.onValueInput $ Just <<< UpdateEmplacement
                       ]
            , HH.label [ HP.for "empl1" ] [HH.text "1"]
            , HH.input [ HP.type_ HP.InputRadio
                       , HP.name "emplacements"
                       , HP.id_ "empl2"
                       , HP.value "2"
                       , HE.onValueInput $ Just <<< UpdateEmplacement
                       ]
            , HH.label [ HP.for "empl2" ] [HH.text "2"]
            , HH.p_ [ HH.text $ "Emplacement " <> show state.emplacement ]
            , HH.p_ [ HH.text "What is your name?" ]
            , hello
            , HH.input [ HP.type_ HP.InputText
                       , HP.value state.name
                       , HP.placeholder "type note"
                       , HP.autofocus true 
                       , HE.onValueInput $ Just <<< UpdateName
                       , onEnter AddTodo
                       ]
            , HH.ul [] $
              (\note -> HH.li [] [HH.label [] [HH.text note.text]]) 
                <$> state.notes

            , HH.button 
              [HE.onClick $ const $ Just $ DeleteAll state.elapsed] 
              [HH.text $ "Clear All"]
            ]

    where
      hello = if state.name == ""
              then HH.p_ []
              else HH.p_ [ HH.text $ "Hello " <> state.name ]

main :: Effect Unit
main = HA.runHalogenAff $
       HA.awaitBody >>= runUI page 0

