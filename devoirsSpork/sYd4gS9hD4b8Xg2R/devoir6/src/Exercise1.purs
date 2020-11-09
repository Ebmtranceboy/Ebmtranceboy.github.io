module Exercise1 where

import Prelude
import Data.Rational(Rational, fromInt, numerator, denominator)
import Data.Array((!!), (..), length)
import Data.Maybe(fromJust)
import Partial.Unsafe(unsafePartial)

import SporKaTeX( t, nl
                , section)
import Rand(Rand, rand, rands)
import Control.Monad.State (State)
import Spork.Html (Html)

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
      indices = (\x -> mod x lp) <$> rands 3 r
      nth arr n = unsafePartial $ fromJust $ arr !! n
      sides = nth possibleSides <$> indices
      ab = nth sides 0
      ac = nth sides 1
      bc = nth sides 2
      triangle = {ab: nth sides 0, ac: nth sides 1, bc: nth sides 2}
   in 
     if ok triangle
       then triangle
       else randomTriangle (rand r)

scalarT :: Triangle -> Rational
scalarT {ab, ac, bc} = fromInt (ab*ab + ac*ac - bc*bc) / fromInt 2

rshow :: Rational -> String
rshow r = 
  let n = numerator r
      d = denominator r
   in if d == 1 
        then show n
        else "\\frac{" <> show n 
                    <> "}{" 
                    <> show d
                    <> "}"

exo1 :: forall eq b a r.
   Discard a => Monoid a => { code :: Int | r }
                 -> (String -> State (Array (Html b)) a)
                    -> eq -> Rand 
                       -> State (Array (Html b)) a

exo1 model m equation r0 = do
  section "Exercice 1"
  let h = do m "H" 
             t " le projeté orthogonal de "
             m "C"
             t " sur "
             m "(AB)."
             nl
             t "On donne "
  t "Soit "
  m "ABC"
  t " un triangle du plan tel que "
  let r1 = rand r0
      t1 = randomTriangle r1
      s1 = scalarT t1
      s2 = scalarT {ab: t1.ab, ac: t1.bc ,bc: t1.ac }
  case s1 < fromInt 0, s2 < fromInt 0 of
       true, _ -> do 
                     m " H, A"
                     t " et "
                     m "B"
                     t " alignés dans cet ordre avec "
                     h
                     let ah = - s1 / fromInt t1.ab
                     m $ "AH=" <> rshow ah
                     t " et "
                     m $ "AB=" <> show t1.ab

       false, true -> do
                     m "A, B" 
                     t " et "
                     m "H"
                     t " alignés dans cet ordre avec "
                     h
                     m $ "AB=" <> show t1.ab
                     t " et "
                     let ah = s1 / fromInt t1.ab
                     m $ "BH=" <> (rshow $ ah - fromInt t1.ab)

       _, _ -> do
                     m "A, H"
                     t " et "
                     m "B"
                     t " alignés dans cet ordre avec "
                     h
                     let ah = s1 / fromInt t1.ab
                     m $ "AH=" <> rshow ah
                     t " et "
                     m $ "HB=" <> (rshow  $ fromInt t1.ab - ah)

                     
  t "."
  nl
  t "Calculer "
  m "\\overrightarrow{AB}\\cdot\\overrightarrow{AC}."
  
  if model.code < 0 
    then do
          nl
          t "réponse: "
          m $ rshow s1
    else pure mempty

