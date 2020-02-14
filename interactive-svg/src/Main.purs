module Main where

import Prelude

import Data.Const (Const)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import SVGpork.Geometry (Circle(..), point)
import SVGpork.Render (render', defaultContext)
import Spork.App (App)
import Spork.App as App
import Spork.Html (Html)
import Spork.Html as H
import Spork.Interpreter (liftNat, merge, never)
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

type Model = { circles :: Array Circle
             , rect :: Maybe Rect
             }

initialModel :: Model
initialModel = {circles: [], rect: Nothing}

update :: Model → Action → App.Transition Probe Model Action
update model = case _ of

  None -> App.purely model

  UpdateCircles mouse -> App.purely $
    case model.rect of
      Just rect ->  model{circles = model.circles
                                <> [Circle {center: point "" ((toNumber $ clientX mouse) - rect.left)
                                                         ((toNumber $ clientY mouse) - rect.top)
                                           , radius: 50.0}]}
      _         -> model

  SetRect rect ->  App.purely model{rect = Just rect}

  TransmitRef ref ->
    let effects = case ref of
                      H.Created el -> App.lift $ Probe (fromElement el) SetRect None
                      _            -> mempty
    in {model, effects}

render :: Model -> Html Action
render model =
  let ctx = defaultContext { stroke = "#050409"}
  in H.div [H.ref (H.always TransmitRef)][
   H.elemWithNS
        (Just $ H.Namespace "http://www.w3.org/2000/svg")
        "svg"
        [ H.attr "width" "800px"
        , H.attr "height" "600px"
        , H.onMouseDown (H.always UpdateCircles)
        ] $
        (model.circles >>= render' ctx)
        <> render' ctx (point "" 800.0 600.0)
        ]

app ∷ App Probe (Const Void) Model Action
app = { update, render, init: App.purely initialModel , subs: const mempty}

main ∷ Effect Unit
main = do
  inst ←
    App.makeWithSelector
      (liftNat runProbe `merge` never)
      app
      "#app"
  inst.run
