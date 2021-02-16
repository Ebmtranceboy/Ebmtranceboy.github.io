module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Date (Date, Month, canonicalDate, diff, weekday)
import Data.Either (either)
import Data.Enum (toEnum)
import Data.Int (round)
import Data.Lens (lens, Lens, over, set, view)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (Pattern (..), split)
import Data.Time.Duration (Days(..))
import Data.Tuple.Nested ((/\), type (/\))
import DOM.HTML.Indexed.InputType (InputType)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (error)
import Global (readFloat)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.EventSource (Finalizer(..), affEventSource, emit) as EventSource
import Halogen.VDom.Driver (runUI)
import Hooks.UseLocalStorage (Key(..), useLocalStorage)
import Simple.JSON (readJSON, writeJSON)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key) as Event

type Note =
  { text ∷ String
  }
  
type DateValues = 
  { year :: Int
  , month :: Int
  , day :: Int 
  }

type Fluid = 
  { eau :: Number
  , edf :: Number 
  }

raz = 
  { eau: 0.0
  , edf: 0.0 
  } :: Fluid

type State = 
  { baseDate :: Maybe DateValues
  , newerDate :: Maybe DateValues
  , emplacement :: Int
  , base :: Array Fluid
  , newer :: Array Fluid
  , name ∷ String 
  , notes ∷ Array Note
  }

initialState ∷ Int -> State
initialState n = 
  { baseDate: Nothing
  , newerDate: Nothing
  , emplacement: n
  , base: { eau: -1.0, edf: -1.0 } Array.: Array.replicate 8 raz
  , newer: { eau: -1.0, edf: -1.0 } Array.: Array.replicate 8 raz
  , name: "" 
  , notes: []
  }

valuesToDate :: Maybe DateValues -> Maybe Date
valuesToDate mvs = mvs >>= \{year, month, day} ->  
   canonicalDate <$> toEnum year <*> toEnum month <*> toEnum day

dateValuesToValue :: Maybe DateValues -> String
dateValuesToValue mvs = 
  fromMaybe "" $ ( \{ year, month, day } -> 
                      show year
                      <> "-"
                      <> (if month < 10 then "0" else "")
                      <> show month
                      <> "-"
                      <> (if day < 10 then "0" else "")
                      <> show day
                  ) <$> mvs
   
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

type HM m = Hooks.HookM m Unit  

empl :: forall a m
  .  MonadEffect m 
  => State /\ ((State -> State) -> HM m) 
  -> Int -> Array (HH.HTML a (HM m))
empl (state /\ modifyState) n = 
  [ HH.input  [ HP.type_ HP.InputRadio
              , HP.name "emplacements"
              , HP.id_ $ "empl" <> show n
              , HP.value $ show n
              , HP.checked $ state.emplacement == n
              , HE.onValueInput \newEmpl -> 
                Just $ modifyState _{ emplacement = round $ readFloat newEmpl }
              ]
  , HH.label [ HP.for $ "empl" <> show n ] [ HH.text $ show n ]
  ]

type PeriodL = Lens State State (Array Fluid) (Array Fluid)
type FluidL = Lens Fluid Fluid Number Number

safeAt :: State -> PeriodL -> FluidL -> Int -> Number
safeAt state period fluid n =
  let xs = view fluid <$> (view (ix n) $ Array.singleton <$> view period state)
  in fromMaybe 1.0 $ xs Array.!! 0

_base :: PeriodL
_base = lens _.base $ _ { base = _ }

_newer :: PeriodL
_newer = lens _.newer $ _ { newer = _ }

_eau :: FluidL
_eau = lens _.eau $ _ { eau = _ }

_edf :: FluidL
_edf = lens _.edf $ _ { edf = _ }

inputFluid :: forall r m
  .  MonadEffect m
  => State /\ ((State -> State) -> HM m)
  -> PeriodL 
  -> FluidL 
  -> Array (HP.IProp ( onBlur :: FocusEvent 
                     , onFocus :: FocusEvent
                     , onInput :: Event     
                     , type :: InputType    
                     , value :: String      
                     | r                  
                     ) (HM m))
inputFluid (state /\ modifyState) period fluid = 
  [ HP.type_ HP.InputNumber
  , HE.onValueInput \strval -> Just do
      let e = state.emplacement
      modifyState $ over period $ over (ix e) $ set fluid $ readFloat strval
  , HE.onFocus \_ -> Just $ 
      modifyState $ over period $ over (ix 0) $ set fluid 1.0
  , HE.onBlur \_ -> Just $ 
      modifyState $ over period $ over (ix 0) $ set fluid (-1.0)
  ]
  <>
  if safeAt state period fluid 0 < 0.0
    then
      [ HP.value $ show
                 $ safeAt state period fluid state.emplacement
      ]
    else []

onEnter ∷ forall i r
  . (Unit -> Maybe i) 
  → HH.IProp (onKeyDown ∷ Event.KeyboardEvent | r) i
onEnter cb = HE.onKeyDown \ev →
  if Event.key ev == "Enter"
    then cb unit
    else Nothing

addNote :: State -> State
addNote state = 
  if state.name == ""
    then state
    else state { name = ""
               , notes = Array.snoc state.notes {text: state.name} 
               }

_EUROS_PER_DAY_ = 2.0 :: Number
_EUROS_PER_m3_  = 3.976 :: Number
_EUROS_PER_kWh_ = 0.067 :: Number

aireDeBellac :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
aireDeBellac = Hooks.component \_ _ -> Hooks.do
  let defaultValue = initialState 1
  stateAndModifier <- useLocalStorage
    { defaultValue
    , fromJson: \str -> either (const defaultValue) identity $ readJSON str
    , toJson: writeJSON
    , key: Key "intStorageExample"
    }
  elapsed /\ elapsedId <- Hooks.useState 0
  
  Hooks.useLifecycleEffect do
    
    subId <- Hooks.subscribe do
      EventSource.affEventSource \emitter -> do
        fiber <- Aff.forkAff $ forever do
          Aff.delay $ Milliseconds 1000.0
          EventSource.emit emitter do
              t <- Hooks.get elapsedId
              Hooks.put elapsedId (t + 1)
          
        pure $ EventSource.Finalizer do
          void $ Aff.killFiber (error "Event source finalized") fiber
    
    pure $ Just do
      Hooks.unsubscribe subId

  let
    state /\ modifyState = stateAndModifier
    consoEau = ( safeAt state _newer _eau state.emplacement
                 - safeAt state _base _eau state.emplacement) * _EUROS_PER_m3_
    consoEdf = ( safeAt state _newer _edf state.emplacement
                 - safeAt state _base _edf state.emplacement) * _EUROS_PER_kWh_
    Days nbDays = fromMaybe (Days 0.0) $ diff <$> valuesToDate state.newerDate 
                                                <*> valuesToDate state.baseDate
    consoEmpl = nbDays * _EUROS_PER_DAY_

  Hooks.pure do
    HH.div_ $ [ HH.p_ [ HH.text $ "Started " <> show elapsed 
                                              <> " seconds ago." ]
              ]
              <>
                (Array.concat $ empl stateAndModifier <$> 1 Array... 8)
              <>
              [ HH.p_ [ HH.text $ "Emplacement " <> show state.emplacement ]
              , HH.p_ [ HH.text $ "Base : " <> prettyDate state.baseDate ]
              , HH.input [ HP.type_ HP.InputDate
                         , HP.value $ dateValuesToValue state.baseDate
                         , HE.onValueInput \iAmADate -> 
                              Just $ modifyState _{ baseDate = prove iAmADate }
                         ]
              , HH.label_ [ HH.text "Eau : "]
              , HH.input $ inputFluid stateAndModifier _base _eau      
              , HH.label_ [ HH.text "Edf : "]
              , HH.input $ inputFluid stateAndModifier _base _edf
              , HH.p_ [ HH.text $ "Newer : " <> prettyDate state.newerDate ]
              , HH.input [ HP.type_ HP.InputDate
                         , HP.value $ dateValuesToValue state.newerDate
                         , HE.onValueInput \iAmADate -> 
                            Just $ modifyState _{ newerDate = prove iAmADate }
                         ]
              , HH.label_ [ HH.text "Eau : "]
              , HH.input $ inputFluid stateAndModifier _newer _eau
              , HH.label_ [ HH.text "Edf : "]
              , HH.input $ inputFluid stateAndModifier _newer _edf
              , HH.h3_ [ HH.text $ "Conso Eau: " <> show consoEau ]
              , HH.h3_ [ HH.text $ "Conso Edf: " <> show consoEdf ]
              , HH.h3_ [ HH.text $ "Coût Emplacement : " <> show consoEmpl ]
              , HH.h2_ [ HH.text $ "Total: " <> show ( consoEau 
                                                     + consoEdf 
                                                     + consoEmpl ) ]
              , HH.p_ [ HH.text "Anything to add?" ]
              
              , composeNote state.name
              , HH.input [ HP.type_ HP.InputText
                        , HP.value state.name
                        , HP.placeholder "type note"
                        , HP.autofocus true 
                        , HE.onValueInput \newName ->
                            Just $ modifyState _{ name = newName }
                        , onEnter \_ -> Just $ modifyState addNote
                        ]
              , HH.ul_ $
                (\note -> HH.li_ [ HH.label_ [HH.text note.text] ]) 
                  <$> state.notes

              , HH.button 
                [ HE.onClick \_ -> Just $ modifyState (const $ initialState 1)
                ] 
                [HH.text $ "Clear All"]
              ]
    where
      composeNote note = if note == ""
                  then HH.p_ []
                  else HH.p_ [ HH.text $ "Press Enter to add: " <> note ]
              
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI aireDeBellac unit body

