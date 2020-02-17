module Main where

import Prelude

import Data.Array (filter, uncons, (\\))
import Data.DateTime.Instant (Instant, unInstant) as Date
import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..), Minutes(..), Seconds(..), convertDuration) as Date
import Effect (Effect)
import Effect.Now (now) as Date
import Effect.Ref as Ref
import Effect.Timer as Timer
import Global (readFloat)
import Math (pi, cos, sin) as Math
import SVGpork.Geometry (Circle(..), length, segment, point, vector)
import SVGpork.Render (render', defaultContext)
import Spork.App (App)
import Spork.App as App
import Spork.EventQueue as EventQueue
import Spork.Html (Html)
import Spork.Html as H
import Spork.Interpreter (Interpreter(..), liftNat, merge)
import Web.HTML.HTMLElement (HTMLElement, fromElement, getBoundingClientRect)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)

data Probe a
  = Probe (Maybe HTMLElement) (Rect -> a) a

runProbe ∷ Probe ~> Effect
runProbe (Probe (Just el) next _) = do
  rect <- getBoundingClientRect el
  pure $ next rect
runProbe (Probe _ _ none) = pure none

type Rect = { left :: Number
            , right :: Number
            , height :: Number
            , top :: Number
            , bottom :: Number
            , width :: Number
            }

data Action = None
  | UpdateCircles MouseEvent
  | TransmitRef H.ElementRef
  | SetRect Rect
  | Tick Date.Instant
  | UpdateTimeUnit Int
  | UpdateTimeFraction String

data TimeUnit = S | M | L

type Model = { circles :: Array Circle
             , rect :: Maybe Rect
             , instant :: Maybe Date.Instant
             , timeUnit :: TimeUnit
             , timeFraction :: Number
             }

data Sub a = Sub (Date.Instant -> a)

derive instance functorSub :: Functor Sub

initialModel :: Model
initialModel = { circles: []
               , rect: Nothing
               , instant: Nothing
               , timeUnit: M
               , timeFraction: 1.0
               }

radius = 50.0 :: Number

update :: Model → Action → App.Transition Probe Model Action
update model = case _ of

  None -> App.purely model

  UpdateCircles mouse -> App.purely $
    case model.rect of
      Just rect ->  let x = (toNumber $ clientX mouse) - rect.left
                        y = (toNumber $ clientY mouse) - rect.top
                        p = point "" x y
                        close (Circle circle) = (length $ vector p circle.center) < radius
                        toBeRemoved = filter close model.circles
                    in case uncons toBeRemoved of
                        Just _ -> model{circles = model.circles \\ toBeRemoved}
                        _      -> model{circles = model.circles
                                          <> [Circle { center: p
                                                     , radius}]}
      _         -> model

  SetRect rect ->  App.purely model{rect = Just rect}

  TransmitRef ref ->
    let effects = case ref of
                      H.Created el -> App.lift $ Probe (fromElement el) SetRect None
                      _            -> mempty
    in {model, effects}

  Tick time -> App.purely model{instant = Just time}

  UpdateTimeUnit index ->
    App.purely model {timeUnit = case index of
                                  0 -> M
                                  1 -> S
                                  _ -> L
                     }

  UpdateTimeFraction str ->
    App.purely model{timeFraction = readFloat str}

getTimeUnit :: Model -> Number
getTimeUnit model =
  case model.timeUnit of
    L -> let  Date.Milliseconds ms =
                maybe (Date.Milliseconds 0.0)
                  (Date.convertDuration <<< Date.unInstant)
                  model.instant
         in ms
    S -> let  Date.Seconds s =
                maybe (Date.Seconds 0.0)
                  (Date.convertDuration <<< Date.unInstant)
                  model.instant
          in s
    M -> let  Date.Minutes minutes =
                maybe (Date.Minutes 0.0)
                  (Date.convertDuration <<< Date.unInstant)
                  model.instant
         in minutes

render :: Model -> Html Action
render model =
  let ctx = defaultContext { stroke = "#050409"}
  in H.div [H.ref (H.always TransmitRef)]
      [ H.elemWithNS
            (Just $ H.Namespace "http://www.w3.org/2000/svg")
            "svg"
            [ H.attr "width" "800px"
            , H.attr "height" "600px"
            , H.onMouseDown (H.always UpdateCircles)
            ] $
            (model.circles >>= render' ctx)
            <>
             (let period = getTimeUnit model * model.timeFraction
                  angle = 2.0 * Math.pi * period
                  x = 150.0 + 60.0 * Math.cos angle
                  y = 150.0 + 60.0 * Math.sin angle
              in render' ctx $ segment (point "" 150.0 150.0) (point "" x y) Nothing)

      , H.select [H.onSelectedIndexChange (H.always UpdateTimeUnit)]
        [ H.option [] [H.text "Minutes"]
        , H.option [] [H.text "Seconds"]
        , H.option [] [H.text "Milliseconds"]
        ]
      , H.input [ H.attr "type" "range"
                , H.attr "min" "-100"
                , H.attr "max" "100"
                , H.attr "value" "1"
                , H.attr "step" "0.01"
                , H.onValueInput (H.always UpdateTimeFraction)
                ]
      , H.label [] [H.text $ show model.timeFraction]
      ]

app ∷ App Probe Sub Model Action
app = { update
      , render
      , init: App.purely initialModel
      , subs: const $ App.lift (Sub Tick)}

runSubscriptions :: forall i. Interpreter Effect Sub i
runSubscriptions = Interpreter $ EventQueue.withAccumArray \queue -> do
    model <- Ref.new []

    let
        tick = do
            now <- Date.now
            Ref.read model >>= traverse_ case _ of
                Sub k -> queue.push (k now)
            queue.run

        commit new = do
            old <- Ref.read model
            Ref.write new model
            case old, new of
                [], _ -> void $ Timer.setInterval 10 tick
                _, _  -> pure unit

            pure unit

    pure commit

main ∷ Effect Unit
main = do
  inst ←
    App.makeWithSelector
      (liftNat runProbe `merge` runSubscriptions)
      app
      "#app"
  inst.run
