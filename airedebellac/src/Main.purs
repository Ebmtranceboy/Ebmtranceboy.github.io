module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Const (Const)
import Data.Date (Date, Month, canonicalDate, diff, weekday)
import Data.Enum (toEnum)
import Data.Either (hush)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.String (Pattern (..), split)
import Data.Time.Duration (Days)
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

type Fluid = { eau :: Number, edf :: Number }

raz = { eau: 0.0, edf: 0.0 } :: Fluid

type Stat = { base :: Fluid, newer :: Fluid }

type State = 
  { name ∷ String 
  , notes ∷ Array Note
  , elapsed :: Int
  , baseDate :: Maybe DateValues
  , newerDate :: Maybe DateValues
  , emplacement :: Int
  , stats :: Array Stat
  }

data Action 
  = Initialize
  | UpdateName String
  | UpdateBase String
  | UpdateNewer String
  | UpdateEmplacement String
  | UpdateBaseEau String
  | UpdateBaseEdf String
  | UpdateNewerEau String
  | UpdateNewerEdf String
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
  , stats: Array.replicate 9 { base: raz, newer: raz }
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
      >>= Storage.setItem storageKey (writeJSON { notes: state.notes
                                                , baseDate: state.baseDate 
                                                , newerDate: state.newerDate
                                                , stats: state.stats 
                                                })

fromStorage :: Effect State
fromStorage = do
  storedModel ←
    DOM.window
      >>= DOM.localStorage
      >>= Storage.getItem storageKey
      >>> map (_ >>= (readJSON :: String -> E { notes :: Array Note
                                              , baseDate :: Maybe DateValues 
                                              , newerDate :: Maybe DateValues 
                                              , stats :: Array Stat
                                              }) >>> hush)
  pure $ case storedModel of
      Nothing → initialState 0
      Just sm → (initialState 0)
        { notes = sm.notes
        , baseDate = sm.baseDate
        , newerDate = sm.newerDate
        , stats = sm.stats
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
  --H.liftEffect fromStorage >>= H.put
  pure unit
  
handleAction ( UpdateName newName ) = 
  H.modify_ _{ name = newName }

handleAction ( UpdateEmplacement newEmpl ) = 
  H.modify_ _{ emplacement = round $ readFloat newEmpl }

handleAction ( UpdateBaseEau newEau ) = do
  st <- H.get
  let e = st.emplacement
      ms = st.stats Array.!! e
  H.modify_ _{ stats = 
    let ms' = (\s -> Array.updateAt e 
                { base: { edf: s.base.edf
                        , eau: readFloat newEau
                        }
                , newer: s.newer
                } st.stats
              ) =<< ms
      in fromMaybe st.stats ms'
      }
  H.get >>= (toStorage >>> H.liftEffect) 


handleAction ( UpdateBaseEdf newEdf ) = do
  st <- H.get
  let e = st.emplacement
      ms = st.stats Array.!! e
  H.modify_ _{ stats = 
    let ms' = (\s -> Array.updateAt e 
                { base: { eau: s.base.eau
                        , edf: readFloat newEdf
                        }
                , newer: s.newer
                } st.stats
              ) =<< ms
      in fromMaybe st.stats ms'
      }
  H.get >>= (toStorage >>> H.liftEffect) 

handleAction ( UpdateNewerEau newEau ) = do
  st <- H.get
  let e = st.emplacement
      ms = st.stats Array.!! e
  H.modify_ _{ stats = 
    let ms' = (\s -> Array.updateAt e 
                { newer: { edf: s.newer.edf
                         , eau: readFloat newEau
                         }
                , base: s.base
                } st.stats
              ) =<< ms
      in fromMaybe st.stats ms'
      }
  H.get >>= (toStorage >>> H.liftEffect) 

handleAction ( UpdateNewerEdf newEdf ) = do
  st <- H.get
  let e = st.emplacement
      ms = st.stats Array.!! e
  H.modify_ _{ stats = 
    let ms' = (\s -> Array.updateAt e 
                { newer: { eau: s.newer.eau
                         , edf: readFloat newEdf
                         }
                , base: s.base
                } st.stats
              ) =<< ms
      in fromMaybe st.stats ms'
      }
  H.get >>= (toStorage >>> H.liftEffect) 

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
        (\ w d m y -> w <> " " <> show d <> "th " <> show (m :: Month) <> " " <> show y)
          <$> Just wday <*> Just day <*> toEnum month <*> Just year
  in fromMaybe "" label
    
prove :: String -> Maybe DateValues
prove str = 
  let ns = split (Pattern "-") str
      int = round <<< readFloat
  in (\ a b c -> {year: int a, month: int b, day: int c}) 
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

safeAt :: Array Stat -> (Stat -> Number) -> Int -> String
safeAt xs ex n =
  let mx = xs Array.!! n
  in maybe "0.0" (\x -> show $ ex x) mx

render :: forall m. State -> H.ComponentHTML Action () m
render state =
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
            , HH.input [ HP.type_ HP.InputNumber
                       , HP.value $ safeAt state.stats (_.base >>> _.eau) state.emplacement
                       , HE.onValueInput $ Just <<< UpdateBaseEau
                       ]
            , HH.label_ [ HH.text "Edf : "]
            , HH.input [ HP.type_ HP.InputNumber
                       , HP.value $ safeAt state.stats (_.base >>> _.edf) state.emplacement
                       , HE.onValueInput $ Just <<< UpdateBaseEdf
                       ]
            , HH.p_ [ HH.text $ "Newer : " <> prettyDate state.newerDate ]
            , HH.input [ HP.type_ HP.InputDate
                       , HP.value $ dateValuesToValue state.newerDate
                       , HE.onValueInput $ Just <<< UpdateNewer
                       ]
            , HH.label_ [ HH.text "Eau : "]
            , HH.input [ HP.type_ HP.InputNumber
                       , HP.value $ safeAt state.stats (_.newer >>> _.eau) state.emplacement
                       , HE.onValueInput $ Just <<< UpdateNewerEau
                       ]
            , HH.label_ [ HH.text "Edf : "]
            , HH.input [ HP.type_ HP.InputNumber
                       , HP.value $ safeAt state.stats (_.newer >>> _.edf) state.emplacement
                       , HE.onValueInput $ Just <<< UpdateNewerEdf
                       ]
            
            , HH.p_ [ HH.text $ "Diff : " 
                              <> (show :: Maybe Days -> String) (diff <$> valuesToDate state.newerDate 
                                            <*> valuesToDate state.baseDate)
                              <> " days"
                    ]
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

