module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Const (Const)
import Data.Date (Date, Month, canonicalDate, diff, weekday)
import Data.Enum (toEnum)
import Data.Either (hush)
import Data.Int (round)
import Data.Lens (lens, Lens, over, set, view)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (Pattern (..), split)
import Data.Time.Duration (Days(..))
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
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.Event.Internal.Types (Event)
import DOM.HTML.Indexed.InputType (InputType)

type Note =
  { text ∷ String
  }
  
type DateValues = 
  { year :: Int
  , month :: Int
  , day :: Int }

type Fluid = 
  { eau :: Number
  , edf :: Number }

raz = 
  { eau: 0.0
  , edf: 0.0 } :: Fluid

type State = 
  { name ∷ String 
  , notes ∷ Array Note
  , elapsed :: Int
  , baseDate :: Maybe DateValues
  , newerDate :: Maybe DateValues
  , emplacement :: Int
  , base :: Array Fluid
  , newer :: Array Fluid
  }
  
type PeriodL = Lens State State (Array Fluid) (Array Fluid)
type FluidL = Lens Fluid Fluid Number Number

data Action
  = Initialize
  | UpdateName String
  | UpdateBase String
  | UpdateNewer String
  | UpdateEmplacement String
  | UpdateFluid PeriodL FluidL String
  | GotFocus PeriodL FluidL
  | LostFocus PeriodL FluidL
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
  , newerDate: Nothing
  , emplacement: 1
  , base: Array.replicate 9 raz
  , newer: Array.replicate 9 raz
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
toStorage state = do
  let json = writeJSON  { notes: state.notes
                        , baseDate: state.baseDate 
                        , newerDate: state.newerDate
                        , base: state.base
                        , newer: state.newer
                        }
  DOM.window
      >>= DOM.localStorage
      >>= Storage.setItem storageKey json

fromStorage :: Effect State
fromStorage = do
  storedModel ←
    DOM.window
      >>= DOM.localStorage
      >>= Storage.getItem storageKey
      >>> map (_ >>= (readJSON :: String -> E { notes :: Array Note
                                              , baseDate :: Maybe DateValues 
                                              , newerDate :: Maybe DateValues 
                                              , base :: Array Fluid
                                              , newer :: Array Fluid
                                              }
                     ) >>> hush
              )
  pure $ case storedModel of
      Nothing → initialState 0
      Just sm → (initialState 0)
        { notes = sm.notes
        , baseDate = sm.baseDate
        , newerDate = sm.newerDate
        , base = sm.base
        , newer = sm.newer
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
  handleAction ( LostFocus _base _eau )
  handleAction ( LostFocus _base _edf )
  handleAction ( LostFocus _newer _eau )
  handleAction ( LostFocus _newer _edf )
  
handleAction ( UpdateName newName ) = 
  H.modify_ _{ name = newName }

handleAction ( UpdateEmplacement newEmpl ) = 
  H.modify_ _{ emplacement = round $ readFloat newEmpl }

handleAction ( UpdateFluid period fluid strval ) = do
  state <- H.get
  let e = state.emplacement
  H.modify_ $ over period $ over (ix e) $ set fluid $ readFloat strval
  H.get >>= (toStorage >>> H.liftEffect) 

handleAction ( GotFocus period fluid ) = do
  H.modify_ $ over period $ over (ix 0) $ set fluid 1.0

handleAction ( LostFocus period fluid ) = do
  H.modify_ $ over period $ over (ix 0) $ set fluid (-1.0)
  
handleAction ( UpdateBase iAmADate ) = do
  H.modify_ _{ baseDate = prove iAmADate }
  H.get >>= (toStorage >>> H.liftEffect) 

handleAction ( UpdateNewer iAmADate ) = do
  H.modify_ _{ newerDate = prove iAmADate }
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

valuesToDate :: Maybe DateValues -> Maybe Date
valuesToDate mvs = mvs >>= \{year, month, day} ->  
   canonicalDate <$> toEnum year <*> toEnum month <*> toEnum day

dateValuesToValue :: Maybe DateValues -> String
dateValuesToValue mvs = 
  fromMaybe "" $ (\{ year, month, day } -> 
                      show year
                      <> "-"
                      <> (if month < 10 then "0" else "")
                      <> show month
                      <> "-"
                      <> (if day < 10 then "0" else "")
                      <> show day) <$> mvs
   
prettyDate :: Maybe DateValues -> String
prettyDate mvs =
  let mdate = valuesToDate mvs
      wday = maybe "" show $ weekday <$> mdate
      label = mvs >>= \{ year, month, day } ->
        (\ w d m y -> w <> " " 
                   <> show d <> "th " 
                   <> show (m :: Month) <> " " 
                   <> show y)
          <$> Just wday <*> Just day <*> toEnum month <*> Just year
  in fromMaybe "" label
    
prove :: String -> Maybe DateValues
prove str = 
  let ns = split (Pattern "-") str
      int = round <<< readFloat
  in (\ a b c -> { year: int a, month: int b, day: int c }) 
    <$> (ns Array.!! 0) <*> (ns Array.!! 1) <*> (ns Array.!! 2)

empl :: forall a. Int -> Array (HH.HTML a Action)
empl n = 
  [ HH.input  [ HP.type_ HP.InputRadio
              , HP.name "emplacements"
              , HP.id_ $ "empl" <> show n
              , HP.value $ show n
              , HE.onValueInput $ Just <<< UpdateEmplacement
              ]
  , HH.label [ HP.for $ "empl" <> show n ] [HH.text $ show n]
  ]

safeAt :: State -> PeriodL -> FluidL -> Int -> Number
safeAt state period fluid n =
  let mx = (view fluid <$> (view (ix n) $ Array.singleton <$> (view period state))) Array.!! 0
  in fromMaybe 0.0 mx

_base :: PeriodL
_base = lens _.base $ _ { base = _ }

_newer :: PeriodL
_newer = lens _.newer $ _ { newer = _ }

_eau :: FluidL
_eau = lens _.eau $ _ { eau = _ }

_edf :: FluidL
_edf = lens _.edf $ _ { edf = _ }

inputFluid :: forall r
  .  State 
  -> PeriodL 
  -> FluidL 
  -> Array (HP.IProp ( onBlur :: FocusEvent 
                     , onFocus :: FocusEvent
                     , onInput :: Event     
                     , type :: InputType    
                     , value :: String      
                     | r                  
                     )  Action)
inputFluid state period fluid = 
  [ HP.type_ HP.InputNumber
  , HE.onValueInput $ Just <<< ( UpdateFluid period fluid )
  , HE.onFocus $ const $ Just $ GotFocus period fluid 
  , HE.onBlur $ const $ Just $ LostFocus period fluid 
  ]
  <>
  if safeAt state period fluid 0 < 0.0
    then
      [ HP.value $ show
                 $ safeAt state period fluid state.emplacement
      ]
    else []

_EUROS_PER_DAY_ = 2.0 :: Number
_EUROS_PER_m3_  = 3.976 :: Number
_EUROS_PER_kWh_ = 0.067 :: Number
    
render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let consoEau = ( safeAt state _newer _eau state.emplacement
                 - safeAt state _base _eau state.emplacement) * _EUROS_PER_m3_
      consoEdf = ( safeAt state _newer _edf state.emplacement
                 - safeAt state _base _edf state.emplacement) * _EUROS_PER_kWh_
      Days nbDays = fromMaybe (Days 0.0) $ diff <$> valuesToDate state.newerDate 
                                                <*> valuesToDate state.baseDate
      consoEmpl = nbDays * _EUROS_PER_DAY_
  HH.div_ $ [ HH.p_ [ HH.text $ "Started " <> show state.elapsed <> " seconds ago." ]
            ]
            <>
              (Array.concat $ empl <$> 1 Array... 8)
            <>
            [ HH.p_ [ HH.text $ "Emplacement " <> show state.emplacement ]
            , HH.p_ [ HH.text $ "Base : " <> prettyDate state.baseDate ]
            , HH.input [ HP.type_ HP.InputDate
                       , HP.value $ dateValuesToValue state.baseDate
                       , HE.onValueInput $ Just <<< UpdateBase
                       ]
            , HH.label_ [ HH.text "Eau : "]
            , HH.input $ inputFluid state _base _eau      
            , HH.label_ [ HH.text "Edf : "]
            , HH.input $ inputFluid state _base _edf
            , HH.p_ [ HH.text $ "Newer : " <> prettyDate state.newerDate ]
            , HH.input [ HP.type_ HP.InputDate
                       , HP.value $ dateValuesToValue state.newerDate
                       , HE.onValueInput $ Just <<< UpdateNewer
                       ]
            , HH.label_ [ HH.text "Eau : "]
            , HH.input $ inputFluid state _newer _eau
            , HH.label_ [ HH.text "Edf : "]
            , HH.input $ inputFluid state _newer _edf
            , HH.h3_ [ HH.text $ "Conso Eau: " <> show consoEau ]
            , HH.h3_ [ HH.text $ "Conso Edf: " <> show consoEdf ]
            , HH.h3_ [ HH.text $ "Coût Emplacement : " <> show consoEmpl ]
            , HH.h2_ [ HH.text $ "Total: " <> show (consoEau + consoEdf + consoEmpl) ] 
            , HH.p_ [ HH.text "Anything to add?" ]
            , addNote
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
      addNote = if state.name == ""
                  then HH.p_ []
                  else HH.p_ [ HH.text $ "Press Enter to add: " <> state.name ]

main :: Effect Unit
main = HA.runHalogenAff $
       HA.awaitBody >>= runUI page 0

