module Exercise5 where

import Prelude
import Data.Array((!!), (..), length)
import Data.Maybe(fromJust)
import Partial.Unsafe(unsafePartial)

import SporKaTeX( t, nl
                , section)
import Rand(Rand, rand)
import Control.Monad.State (State)
import Spork.Html (Html)

possibleValues :: Array Int
possibleValues = 2..13

type Triangle = {ab :: Int, ac :: Int, t :: Boolean}

randomTriangle :: Rand -> Triangle
randomTriangle r = 
  let r1 = rand r
      r2 = rand r1
      r3 = rand r2
      nth arr n = unsafePartial $ fromJust $ arr !! n
      lp = length possibleValues

   in {ab: nth possibleValues (r1.val `mod` lp)
      ,ac: nth possibleValues (r2.val `mod` lp)
      ,t: r.val `mod` 2 == 0}

scalarT :: Triangle -> Int 
scalarT {ab, ac, t} =
  if t 
    then ab * ac
    else - ab * ac

exo5 :: forall eq b a r.
   Discard a => Monoid a => { code :: Int | r }
                 -> (String -> State (Array (Html b)) a)
                    -> eq -> Rand 
                       -> State (Array (Html b)) a

exo5 model m equation r0 = do
  section "Exercice 5"
  let r1 = rand r0
      t1 = randomTriangle r1
      s1 = scalarT t1
  t "Soit "
  m "ABC"
  t " un triangle tel que "
  m $ "AB = " <> show t1.ab
  t ", "
  m $ "AC = " <> (show $ 2 * t1.ac)
  t " et "
  m $ "\\widehat{BAC}=" <> if t1.t then "\\frac{\\pi}{3}" else "\\frac{2\\pi}{3}"
  t "."
  nl
  t "Calculer "
  m "\\overrightarrow{AB}\\cdot\\overrightarrow{AC}."
  
  if model.code < 0 
    then do
          nl
          t "réponse: "
          m $ show s1
    else pure mempty

