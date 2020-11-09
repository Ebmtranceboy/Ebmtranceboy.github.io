module Exercise2 where

import Prelude
import Data.Array((!!), (..), length)
import Data.Maybe(fromJust)
import Partial.Unsafe(unsafePartial)
import Control.Monad.State (State)
import Spork.Html (Html)

import SporKaTeX( t, nl
                , section)
import Rand(Rand, rand, rands)

possibleValues :: Array Int
possibleValues = -13..13

type Point = {x :: Int, y :: Int}
type Triangle = {a :: Point, b :: Point, c :: Point}

randomTriangle :: Rand -> Triangle
randomTriangle r = 
  let nth arr n = unsafePartial $ fromJust $ arr !! n
      lp = length possibleValues
      indices = (\x -> mod x lp) <$> rands 6 r
      vals = nth possibleValues <$> indices

   in {a: {x: nth vals 0, y: nth vals 1}
      ,b: {x: nth vals 2, y: nth vals 3}
      ,c: {x: nth vals 4, y: nth vals 5}}

scalarT :: Triangle -> Int 
scalarT {a, b, c} =
  let ab = {x: b.x - a.x, y: b.y - a.y}
      ac = {x: c.x - a.x, y: c.y - a.y}
   in ab.x * ac.x + ab.y * ac.y

exo2 :: forall eq b a r.
   Discard a => Monoid a => { code :: Int | r }
                 -> (String -> State (Array (Html b)) a)
                    -> eq -> Rand 
                       -> State (Array (Html b)) a

exo2 model m equation r0 = do
  section "Exercice 2"
  let r1 = rand r0
      t1 = randomTriangle r1
      s1 = scalarT t1
  t "Soient "
  m $ "A(" <> show t1.a.x <> ";" <> show t1.a.y <> ")"
  t ", "
  m $ "B(" <> show t1.b.x <> ";" <> show t1.b.y <> ")"
  t " et "
  m $ "C(" <> show t1.c.x <> ";" <> show t1.c.y <> ")"
  t " trois points du plan dans un repère orthonormé."
  nl
  t "Calculer "
  m "\\overrightarrow{AB}\\cdot\\overrightarrow{AC}."
  
  if model.code < 0 
    then do
          nl
          t "réponse: "
          m $ show s1
    else pure mempty

