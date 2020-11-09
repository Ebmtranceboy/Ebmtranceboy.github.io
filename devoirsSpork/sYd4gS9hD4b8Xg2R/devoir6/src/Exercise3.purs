module Exercise3 where

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

type Triangle = Int

randomTriangle :: Rand -> Triangle
randomTriangle r =
  let lp = length possibleValues
      index = mod (rand r).val lp
    in 2 * (unsafePartial $ fromJust $ possibleValues !! index)

scalarT :: Triangle -> Int 
scalarT c = c*c / 2

exo3 :: forall eq b a r.
   Discard a => Monoid a => { code :: Int | r }
                 -> (String -> State (Array (Html b)) a)
                    -> eq -> Rand 
                       -> State (Array (Html b)) a

exo3 model m equation r0 = do
  section "Exercice 3"
  let r1 = rand r0
      t1 = randomTriangle r1
      s1 = scalarT t1
  t "Soit "
  m "ABC" 
  t " un triangle équilatéral de côté "
  m $ show t1
  t ". "
  nl
  t "Calculer "
  m "\\overrightarrow{AB}\\cdot\\overrightarrow{AC}."
  
  if model.code < 0 
    then do
          nl
          t "réponse: "
          m $ show s1
    else pure mempty

