module Main where

import Prelude

import Data.Const (Const)
import Effect (Effect)
import Control.Monad.State(State)
import Data.Array(replicate, (!!))
import Data.Foldable(foldr)
import Data.Maybe(maybe, fromJust)
import Data.Number(fromString)
import Data.Int(floor)
import Data.Ord(abs)
import Partial.Unsafe(unsafePartial)

import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (liftNat, merge, never)

import SporKaTeX( RenderEffect(..), runRenderEffect
                , fromIncremental, put, get) as KaTeX
import SporKaTeX( t, nl
                , setTitle
                , mathEquation, mathInline)
import Rand(Rand, rand, rands)
import Exercise1 (exo1)
import Exercise2 (exo2)
import Exercise3 (exo3)
import Exercise4 (exo4)

type Model =
  { disabled :: Boolean
  , code :: Int
  , seeds :: Array Rand
  }

initialModel ∷ Model
initialModel =
  { disabled: true
  , code: 0
  , seeds: []
  }

data Action
  = None
  | RenderElement String H.ElementRef
  | RenderContent String

update ∷ Model → Action
       → App.Transition KaTeX.RenderEffect Model Action
update model = case _ of
  None →
    App.purely model

  RenderContent cmd ->
    App.purely $ maybe model (\ nb->
      let code = floor nb
          odd = 2 * abs code + 1
          r0 = rand { val: odd
                    , gen: 0
                    , seed: odd*odd
                    }
          seeds = (\s -> rand {val: s, gen: 0, seed: s*s})
                   <$> ((_ + 1) <<< (_ * 2)) <$> rands 4 r0
      in model { disabled = false
               , code = code
               , seeds = seeds
               })
               $ fromString cmd

  RenderElement str ref ->
    let
      effects = case ref of
        H.Created el → App.lift (KaTeX.RenderEffect str el None)
        H.Removed _  → mempty
    in
      { model, effects }


render ∷ Model → H.Html Action
render model =
  H.div
    []
    [ header
    , content model
    , command


    ]

header :: H.Html Action
header = H.div [] $  KaTeX.fromIncremental $ do
  let m t = mathInline RenderElement t
  setTitle "Devoir 8 : Produit scalaire / Suites numériques"
  t "Nom:"
  m $ spacex 40
  t "Prénom:"
  m $ spacex 40
  t "Classe:"
  nl

  KaTeX.put $ H.ul []
      [ H.li [] [H.text "4 exercices"]
      , H.li [] (KaTeX.fromIncremental $ do
           t "5 points par exercice ("
           m "\\bullet"
           t ": 1 point, "
           m "\\circ"
           t ": "
           m "\\frac{1}{2}"
           t " point)"
           KaTeX.get)
      , H.li [] [H.text "sans document"]
      , H.li [] [H.text "calculatrice nécessaire"]
      ]
  KaTeX.get

content :: Model -> H.Html Action
content model =
  if model.disabled
    then H.empty
    else H.div [] $ KaTeX.fromIncremental $ lines model

command :: H.Html Action
command = H.input [ H.autofocus true
                  , H.onValueChange (H.always RenderContent)]

texLeaf :: String -> H.Html Action
texLeaf str = H.label [H.ref (H.always (RenderElement str))] []


lines :: Model -> State (Array (H.Html Action))
            (Array (H.Html Action))
lines model = do
  let m = mathInline RenderElement
      equation = mathEquation RenderElement
  exo1 model m equation $ unsafePartial $ fromJust $ model.seeds !! 0
  exo2 model m equation $ unsafePartial $ fromJust $ model.seeds !! 1
  exo3 model m equation $ unsafePartial $ fromJust $ model.seeds !! 2
  exo4 model m equation $ unsafePartial $ fromJust $ model.seeds !! 3
  nl
  nl
  KaTeX.get

app ∷ App.App KaTeX.RenderEffect (Const Void) Model Action
app =
  { render
  , update
  , subs: const mempty
  , init: App.purely initialModel
  }

spacex :: Int -> String
spacex n = foldr (<>) "" $ replicate n "\\;"

main ∷ Effect Unit
main = do
  inst ←
    App.makeWithSelector
      (liftNat KaTeX.runRenderEffect `merge` never)
      app
      "#app"
  inst.run
