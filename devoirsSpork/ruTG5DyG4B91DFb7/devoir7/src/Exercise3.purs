module Exercise3 where

import Prelude

import Control.Monad.State (State)
import Rand (Rand, rand)
import SporKaTeX (t, nl, section)
import Spork.Html (Html)
import Data.Rational(Rational, fromInt, numerator, denominator)

majorant :: Int
majorant = 10

randomInteger :: Rand -> Int
randomInteger r =
  let rsign = rand r
      sign = 2 * (mod rsign.val 2) - 1
      ri = rand rsign
      i = mod ri.val majorant
  in sign * i

randomPositive :: Rand -> Int
randomPositive r =
  let ri = rand r
      i = mod ri.val majorant
  in i

type Param = {a :: Int, b :: Int, c :: Int}

randomParam :: Rand -> Param
randomParam r =
  let ra = rand r
      a = randomInteger ra
      rb = rand ra
      b = randomInteger rb
      rc = rand rb
      c = randomPositive rc
      ok = a /= 0 && b /= 0 && c /= 0
    in if ok
        then {a,b,c}
        else randomParam rc

showSeq :: Param -> String
showSeq {a,b,c} =
  (case a of
    -1 -> "-n"
    1 -> "n"
    _ -> show a <> "n") <> (
    if b < 0
      then "-" <> "\\dfrac{" <> show (-b) <> "}{n+" <> show c <> "}"
      else "+" <> "\\dfrac{" <> show b <> "}{n+" <> show c <> "}")

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

exo3 :: forall eq b r.
   { code :: Int | r }
                 -> (String -> State (Array (Html b)) Unit)
                    -> eq -> Rand 
                       -> State (Array (Html b)) Unit
exo3 model m equation r0 = do
  section "Exercice 3"
  t "Soit "
  m "(w_n)"
  t " la suite définie sur "
  m "\\mathbb{N}"
  t " par "
  let {a,b,c} = randomParam r0
  m $ "w_n = " <> showSeq {a,b,c}
  t "."
  nl
  nl
  m $ "w_0=" <> spacing <> "w_1=" <> spacing <> "w_2=" <> spacing <> "w_3="

  if model.code < 0
    then do
          nl
          t "réponse: "
          m $ "[" <> (rshow $ fromInt b / fromInt c) <> ","
                  <> (rshow $ fromInt a + fromInt b / (fromInt $ 1 + c)) <> ","
                  <> (rshow $ fromInt (2*a) + fromInt b / (fromInt $ 2 + c)) <> ","
                  <> (rshow $ fromInt (3*a) + fromInt b / (fromInt $ 3 + c)) <> "]"
          t ""
    else pure mempty
