module Main where

import Prelude

import CSS (fontSize, em, body, (?), color, label, white, key, Prefixed (..), sup, display, grid, fontFamily, sansSerif, backgroundImage, backgroundRepeat, backgroundSize, noRepeat, vh, cover, height, black)
import CSS (Key(..), fromString) as CSS
import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Date (Date, Month, canonicalDate, diff, weekday)
import Data.Either (either)
import Data.Enum (toEnum)
import Data.Int (round, toNumber, floor)
import Data.Lens (lens, Lens, over, set, view)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.NonEmpty (singleton) as NonEmpty
import Data.Number (fromString)
import Data.String (Pattern (..), split)
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import DOM.HTML.Indexed.InputType (InputType)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Hooks.UseLocalStorage (Key(..), useLocalStorage)
import Simple.JSON (readJSON, writeJSON)
import Web.DOM.ParentNode (QuerySelector(..))
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
valuesToDate mvs = mvs >>= \{ year, month, day } ->  
   canonicalDate <$> toEnum year <*> toEnum month <*> toEnum day

showTwoDigitsInt :: Int -> String
showTwoDigitsInt n = 
  (if n < 10 
     then "0"
     else "") <> show n
   
dateValuesToValue :: Maybe DateValues -> String
dateValuesToValue mvs = 
  fromMaybe "" $ ( \{ year, month, day } -> 
                      show year
                      <> "-"
                      <> showTwoDigitsInt month
                      <> "-"
                      <> showTwoDigitsInt day
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
      int s = maybe 0 round $ fromString s
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
              , HP.id $ "empl" <> show n
              , HP.value $ show n
              , HP.checked $ state.emplacement == n
              , HE.onValueInput \newEmpl -> 
                modifyState _{ emplacement = maybe 0 round $ fromString newEmpl }
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
  , HE.onValueInput \strval -> do
      let e = state.emplacement
      modifyState $ over period $ over (ix e) $ set fluid $ (\s -> fromMaybe 0.0 $ fromString s) strval
  , HE.onFocus \_ -> 
      modifyState $ over period $ over (ix 0) $ set fluid 1.0
  , HE.onBlur \_ ->  
      modifyState $ over period $ over (ix 0) $ set fluid (-1.0)
  ]
  <>
  if safeAt state period fluid 0 < 0.0
    then
      [ HP.value $ show
                 $ safeAt state period fluid state.emplacement
      ]
    else []

unlessEnter ∷ forall i r
  . (Unit -> i) 
  → (Unit -> i) 
  → HH.IProp (onKeyDown ∷ Event.KeyboardEvent | r) i
unlessEnter defaultCb cb = HE.onKeyDown \ev →
  if Event.key ev == "Enter"
    then cb unit
    else defaultCb unit

addNote :: State -> State
addNote state = 
  if state.name == ""
    then state
    else state { name = ""
               , notes = Array.snoc state.notes {text: state.name} 
               }

twoDigits :: Number -> Number
twoDigits x = 
  (toNumber $ round $ x * 100.0) / 100.0

showTwoDigits :: Number -> String
showTwoDigits x = 
  let e = floor x
      d = round $ (x - toNumber e) * 100.0
  in show e <> "," <> showTwoDigitsInt d
  
_EUROS_PER_DAY_ = 2.0 :: Number
_EUROS_PER_m3_  = 3.976 :: Number
_EUROS_PER_kWh_ = 0.067 :: Number

contentComponent :: forall q i o m. MonadAff m => H.Component q i o m
contentComponent = Hooks.component \_ _ -> Hooks.do
  let defaultValue = initialState 1
  stateAndModifier <- useLocalStorage
    { defaultValue
    , fromJson: \str -> either (const defaultValue) identity $ readJSON str
    , toJson: writeJSON
    , key: Key "intStorageExample"
    }
  elapsed /\ elapsedId <- Hooks.useState 0
  
  Hooks.useLifecycleEffect do
    { emitter, listener } <- H.liftEffect HS.create
    subId <- Hooks.subscribe emitter
    H.liftEffect $ HS.notify listener do
      void $ Hooks.fork $ forever do
        void $ liftAff $ Aff.delay $ Milliseconds 1000.0
        t <- Hooks.get elapsedId
        Hooks.put elapsedId (t + 1)
    
    pure $ Just do
      Hooks.unsubscribe subId

  let
    state /\ modifyState = stateAndModifier
    consoEau = twoDigits $ ( safeAt state _newer _eau state.emplacement
                            - safeAt state _base _eau state.emplacement) * _EUROS_PER_m3_
    consoEdf = twoDigits $ ( safeAt state _newer _edf state.emplacement
                 - safeAt state _base _edf state.emplacement) * _EUROS_PER_kWh_
    Days nbDays = fromMaybe (Days 0.0) $ diff <$> valuesToDate state.newerDate 
                                                <*> valuesToDate state.baseDate
    consoEmpl = twoDigits $ nbDays * _EUROS_PER_DAY_

  Hooks.pure do
    HH.div_ $ [ HH.p
                [  HC.style do
                    fontSize $ em 2.0
                ]
                [ HH.text $ "Started " <> show elapsed 
                                       <> " seconds ago." 
                ]
              ]
              <>
                (Array.concat $ empl stateAndModifier <$> 1 Array... 8)
              <>
              [ HH.p_ [ HH.text $ "Emplacement " <> show state.emplacement ]
              , HH.p_ 
                [ HH.i [ HP.class_ $ HH.ClassName "far fa-calendar-check"] []
                , HH.text $ " Check-in : " <> prettyDate state.baseDate 
                ]
              , HH.div 
                [ HC.style do
                    display grid
                    key (CSS.Key $ Plain "grid-template-columns") "1fr 1fr 1fr;"
                ]
                [ HH.div_
                  [ HH.input  [ HP.type_ HP.InputDate
                              , HP.value $ dateValuesToValue state.baseDate
                              , HE.onValueInput \iAmADate -> 
                                    modifyState _{ baseDate = prove iAmADate }
                              ]
                  ]
                , HH.div_ 
                  [ HH.label_ 
                    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-shower"] []
                    , HH.text " Eau : "
                    ]
                  , HH.input $ inputFluid stateAndModifier _base _eau
                  , HH.label_ [ HH.text "m" ]
                  , HH.sup_ [ HH.text "3" ]
                  ]
                , HH.div_
                  [ HH.label_ 
                    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-charging-station"] []
                    , HH.text " Edf : "
                    ]
                  , HH.input $ inputFluid stateAndModifier _base _edf
                  , HH.label_ [ HH.text "kWh" ]
                  ]
                ]
              , HH.p_ 
                [ HH.i [ HP.class_ $ HH.ClassName "far fa-calendar"] []
                , HH.text $ " Check-out : " <> prettyDate state.newerDate 
                ]
              , HH.div 
                [ HC.style do
                    display grid
                    key (CSS.Key $ Plain "grid-template-columns") "1fr 1fr 1fr;"
                ]
                [ HH.div_
                  [ HH.input  [ HP.type_ HP.InputDate
                              , HP.value $ dateValuesToValue state.newerDate
                              , HE.onValueInput \iAmADate -> 
                                  modifyState _{ newerDate = prove iAmADate }
                              ]
                  ]
                , HH.div_
                  [ HH.label_ 
                    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-shower"] []
                    , HH.text " Eau : "
                    ]
                  , HH.input $ inputFluid stateAndModifier _newer _eau
                  , HH.label_ [ HH.text "m" ]
                  , HH.sup_ [ HH.text "3" ]
                  ]
                , HH.div_
                  [ HH.label_ 
                    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-charging-station"] []
                    , HH.text " Edf : "
                    ]
                  , HH.input $ inputFluid stateAndModifier _newer _edf
                  , HH.label_ [ HH.text "kWh" ]
                  ]
                ]
              , HH.div 
                [ HC.style do
                    display grid
                    key (CSS.Key $ Plain "grid-template-columns") "1fr 1fr 1fr;"
                ]
                [ HH.h3_ 
                  [ HH.i  [ HP.class_ $ HH.ClassName "fas fa-bed"] []
                          , HH.text $ " Coût Emplacement : " 
                              <> showTwoDigits consoEmpl  <> " €" 
                  ]
                , HH.h3_ 
                  [ HH.i  [ HP.class_ $ HH.ClassName "fas fa-shower"] []
                          , HH.text $ " Coût Eau : " 
                              <> showTwoDigits consoEau <> " €" 
                  ]
                , HH.h3_ 
                  [ HH.i  [ HP.class_ $ HH.ClassName "fas fa-charging-station"] []
                          , HH.text $ " Coût Edf : " 
                              <> showTwoDigits consoEdf <> " €" 
                  ]
                ]
              , HH.h2_ 
                [ HH.i 
                  [ HP.class_ $ HH.ClassName "fas fa-cash-register"] []
                , HH.text $ " Total : " <> showTwoDigits ( consoEau 
                                                     + consoEdf 
                                                     + consoEmpl )  <> " €"
                ]
              , HH.p_ [ HH.text "Anything to add?" ]
              
              , composeNote state.name
              , HH.input  [ HP.type_ HP.InputText
                          , HP.value state.name
                          , HP.placeholder "type note"
                          , HP.autofocus true 
                          , HE.onValueInput \newName ->
                              modifyState _{ name = newName }
                          , unlessEnter (\_ -> modifyState identity) \_ -> modifyState addNote
                          ]
              , HH.ul_ $
                (\note -> HH.li_ [ HH.label_ [HH.text note.text] ]) 
                  <$> state.notes

              , HH.button 
                [ HE.onClick \_ -> modifyState (const $ initialState 1)
                ] 
                [ HH.text $ "Clear All" ]
              ]
    where
      composeNote note = if note == ""
                  then HH.p_ []
                  else HH.p_ [ HH.text $ "Press Enter to add: " <> note ]
              

styleComponent :: forall q i o m. MonadAff m => H.Component q i o m
styleComponent = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HC.stylesheet $ body ? do
                        backgroundImage $ CSS.fromString "linear-gradient(rgba(140,140,140,0.8), rgba(0,0,0,0.8)),url(aagdv_bellac.png)"
                        height $ vh 100.0
                        color white
                        backgroundRepeat noRepeat
                        backgroundSize cover
                        fontFamily [ "Raleway" ] $ NonEmpty.singleton sansSerif
                        label ? do 
                          color black
                        sup ? color black
scriptComponent :: forall q i o m. MonadAff m => H.Component q i o m
scriptComponent = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.script
      [ HP.src "https://kit.fontawesome.com/4958828633.js"
      , HP.prop (HH.PropName "crossorigin") "anonymous"
      ]
      [
      ]

linkComponent :: forall q i o m. MonadAff m => H.Component q i o m
linkComponent = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.link 
      [ HP.rel "stylesheet"
      , HP.href "https://fonts.googleapis.com/css?family=Raleway"
      ]
      
main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  let drivenBy tag component = 
        traverse_ (runUI component unit) =<< HA.selectElement (QuerySelector tag)
  "head" `drivenBy` linkComponent
  "head" `drivenBy` scriptComponent
  "head" `drivenBy` styleComponent
  "body" `drivenBy` contentComponent
  
  
