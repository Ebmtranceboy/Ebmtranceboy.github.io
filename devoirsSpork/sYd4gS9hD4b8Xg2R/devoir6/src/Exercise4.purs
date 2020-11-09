module Exercise4 where

import Prelude
import Data.Array((!!), (..), length)
import Data.Maybe(fromJust)
import Partial.Unsafe(unsafePartial)
import Control.Monad.State (State)
import Spork.Html (Html)

import SporKaTeX( t, nl
                , section)
import Rand(Rand, rand, unsort)

possibleSides :: Array Int
possibleSides = 2..23

type Triangle = {ab :: Int, ac :: Int, bc :: Int}

randomTriangle :: Rand -> Triangle
randomTriangle r =
  let smaller a b c = a <= b + c
      ok {ab: a, ac: b, bc: c} = smaller a b c 
                              && smaller b c a 
                              && smaller c a b
                              && (a + b + c) `mod` 2 == 0
                              && a*a + b*b /= c*c
                              && a*a + c*c /= b*b
                              && b*b + c*c /= a*a 
      lp = length possibleSides
      indices = (\x -> mod x lp) <$> unsort lp r
      nth arr n = unsafePartial $ fromJust $ arr !! n
      sides = nth possibleSides <$> indices
      ab = nth sides 0
      ac = nth sides 1
      bc = nth sides 2
      triangle = {ab, ac, bc}
   in 
     if ok triangle
       then triangle
       else randomTriangle (rand r)

scalarT :: Triangle -> Int 
scalarT {ab, ac, bc} = (ab*ab + ac*ac - bc*bc) / 2

exo4 :: forall eq b a r.
   Discard a => Monoid a => { code :: Int | r }
                 -> (String -> State (Array (Html b)) a)
                    -> eq -> Rand 
                       -> State (Array (Html b)) a

exo4 model m equation r0 = do
  section "Exercice 4"
  t "Soit "
  m "ABC"
  t " tel que "
  let r1 = rand r0
      t1 = randomTriangle r1
      s1 = scalarT t1
  m $ "AB = " <> show t1.ab
  t ", "
  m $ "AC = " <> show t1.ac
  t " et "
  m $ "BC = " <> show t1.bc
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

