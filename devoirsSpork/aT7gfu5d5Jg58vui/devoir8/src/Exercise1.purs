module Exercise1 where

import Prelude

import Control.Monad.State (StateT)
import Data.Array (length, (!!))
import Data.Identity (Identity)
import Data.Int (pow)
import Data.Maybe (fromJust)
import Data.Rational (Rational, fromInt, numerator, denominator)
import Partial.Unsafe (unsafePartial)
import Rand (Rand, rand)
import SporKaTeX (nl, section, b, em, t)
import Spork.Html (Html)

majorant :: Int
majorant = 20

type Acute = Boolean -- is angle pi/3 or 2pi/3
type Param = {ac :: Int, ab :: Int, sqrtD :: Int, a :: Acute}

problems :: Array {ac :: Int, ab :: Int, sqrtD :: Int}
problems = [ {ac: 7, ab: 3, sqrtD: 13}, {ac: 7, ab: 5, sqrtD: 11}
           , {ac: 13, ab: 7, sqrtD: 23}, {ac: 13, ab: 8, sqrtD: 22}
           , {ac: 14, ab: 6, sqrtD: 26}, {ac: 14, ab: 10, sqrtD: 22}
           , {ac: 19, ab: 5, sqrtD: 37}, {ac: 19, ab: 16, sqrtD: 26}
           , {ac: 21, ab: 9, sqrtD: 39}, {ac: 21, ab: 15, sqrtD: 33}
           , {ac: 26, ab: 14, sqrtD: 46}, {ac: 26, ab: 16, sqrtD: 44}
           , {ac: 28, ab: 12, sqrtD: 52}, {ac: 28, ab: 20, sqrtD: 44}
           , {ac: 31, ab: 11, sqrtD: 59}, {ac: 31, ab: 24, sqrtD: 46}
           , {ac: 35, ab: 15, sqrtD: 65}, {ac: 35, ab: 25, sqrtD: 55}
           , {ac: 37, ab: 7, sqrtD: 73}, {ac: 37, ab: 33, sqrtD: 47}
           , {ac: 38, ab: 10, sqrtD: 74}, {ac: 38, ab: 32, sqrtD: 52}
           , {ac: 39, ab: 21, sqrtD: 69}, {ac: 39, ab: 24, sqrtD: 66}]

randomParam :: Rand -> Param
randomParam r =
  let rp = rand r
      {ac, ab, sqrtD} = unsafePartial $ fromJust $
                          problems !! (rp.val `mod` (length problems))
      ra = rand rp
      a = ra.val `mod` 2 == 0
    in {ac,ab,sqrtD,a}

showAngle :: Acute -> String
showAngle = case _ of
  true -> "\\frac{\\pi}{3}"
  false -> "\\frac{2\\pi}{3}"

infix 8 pow as **

showInt :: Int -> String
showInt n =
  if n < 0
    then show n
    else "+" <> show n

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

spacing = "\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;" :: String

exo1 :: forall t40 t78 t85.
  Discard t78 => { code :: Int
                 | t85
                 }
                 -> (String -> StateT (Array (Html t40)) Identity t78)
                    ->  (String -> StateT (Array (Html t40)) Identity t78)
                       -> { gen :: Int
                          , seed :: Int
                          , val :: Int
                          }
                          -> StateT (Array (Html t40)) Identity Unit
exo1 model m equation r0 = do
  section "Exercice 1"
  t "Soit "
  m "ABC"
  t " un triangle tel que "
  let {ac,ab,sqrtD,a} = randomParam r0
  m $ "AB = " <> show ab
  t ", "
  m $ "AC = " <> show ac
  t " et "
  m $ "\\widehat{ABC} = " <> showAngle a <> "."
  nl
  t "On note "
  m "BC=x."
  nl
  nl

  b "1"
  m "\\bullet\\bullet\\circ\\;"
  t "Evaluer, en fonction de  "
  m "x"
  t ", le produit scalaire "
  m "\\overrightarrow{BA}\\cdot\\overrightarrow{BC}"
  t " de "
  em "deux"
  t " manières différentes."
  nl
  t "En déduire que "
  m "x"
  t " satisfait l'équation "
  equation $ "x^2" <> (if a then "-" else "+") <> show ab <> "x" <> showInt (ab**2-ac**2) <> "=0"
  nl
  nl

  b "2"
  m "\\bullet\\bullet\\circ\\;"
  t "Résoudre cette équation, et en déduire la valeur de "
  m "BC."
  nl

  if model.code < 0
    then do
          nl
          t "réponse: "
          t $ rshow $ ((fromInt $ (if a then ab else -ab) + sqrtD)) / (fromInt 2)
    else pure mempty
