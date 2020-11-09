module Main where

import Prelude
import Effect (Effect)
import Partial.Unsafe(unsafePartial)
import KaTeX (cat, list, newline, raw, render
             , setTitle, subraw, subrender, section, bold, equation)
import DOM.Editor as DOM
import Data.Array(replicate)
import Data.Foldable(foldr)
import Data.Ord(abs) as Ord
import Rand (rand)
import First(first)
import Second(second)
import Third(third)

foreign import fromString :: String -> Int 

cb :: DOM.Document -> DOM.Event -> Effect Unit
cb doc = unsafePartial $ \ev -> do
  val <- DOM.inputedValueFromEvent ev
  let mode = fromString val < 0
  let odd = 2 * (Ord.abs $ fromString val) + 1
  let r0 = rand {val: odd, gen: 0, seed: odd * odd}
  newline
  
  list [ cat [subraw "4 exercices"] 
       , cat [subraw "5 points par exercice "
             , subrender "(\\bullet"
             , subraw " : 1 point, "
             , subrender "\\circ : \\frac{1}{2}"
             , subraw " point)"]
       , cat [ subraw "qualité de la rédaction "
             , subraw "prise en compte"]
       , cat [subraw "sans document"]
       , cat [subraw "calculatrice autorisée"]]
  
  newline
  
  section "Exercice 1"
  let r1 = rand r0
  first r1 doc mode
  
  section "Exercice 2"
  let r2 = rand r1
  second r2 doc mode
  
  section "Exercice 3"
  let r3 = rand r2
  third r3 doc mode
  
  section "Exercice 4"
  bold "1••◦"
  raw " Résoudre l'inéquation"
  equation "x+\\dfrac{1}{x}-2\\geq 0."
  
  newline
  bold "2◦"
  raw " En déduire que, pour tout réel "
  render "a>0, \\quad a+\\dfrac{1}{a}\\geq 2."
  
  newline
  bold "3••"
  raw " Montrer alors que, pour tous réels strictement positifs "
  render "\\alpha"
  raw " et "
  render "\\beta"
  raw ", on a"
  newline
  render "\\quad"
  bold "a) "
  render "\\quad \\dfrac{\\alpha}{\\beta}+\\dfrac{\\beta}{\\alpha}\\geq 2"
  newline
  render "\\quad"
  bold "b)"
  render "\\quad (\\alpha+\\beta)(\\dfrac{1}{\\alpha}+\\dfrac{1}{\\beta})\\geq 4."
  
spacex :: Int -> String 
spacex n = foldr (<>) "" $ replicate n "\\;"

main :: Effect Unit
main = void $ unsafePartial do
  setup <- DOM.setup
  
  seed <- DOM.createElement "input" setup.document
  _ <- DOM.addEventListener (cb setup.document) DOM.change seed
  _ <- DOM.appendChild seed setup.body
  
  setTitle "Devoir 5 : Probabilités conditionnelles / Fonctions de degré 2"
  raw "Nom:"
  render $ spacex 40
  raw "Prénom:"
  render $ spacex 40
  raw "Classe:"

  _ <- DOM.focus seed
  pure unit
  
