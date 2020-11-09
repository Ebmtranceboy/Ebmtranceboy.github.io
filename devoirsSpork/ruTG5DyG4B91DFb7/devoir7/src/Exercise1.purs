module Exercise1 where

import Prelude

import Control.Monad.State (State)
import Rand (Rand, rand)
import SporKaTeX (t, nl, section)
import Spork.Html (Html)

majorant :: Int
majorant = 10

randomInteger :: Rand -> Int
randomInteger r =
  let rsign = rand r
      sign = 2 * (mod rsign.val 2) - 1
      ri = rand rsign
      i = mod ri.val majorant
  in sign * i

type Param = {a :: Int, b :: Int}

randomParam :: Rand -> Param
randomParam r =
  let ra = rand r
      a = randomInteger ra
      rb = rand ra
      b = randomInteger rb
      ok = a /= 1 && a /= 0 && b /= 0
    in if ok
        then {a,b}
        else randomParam rb

showSeq :: Param -> String
showSeq {a,b} =
  (case a of
    -1 -> "-n"
    1 -> "n"
    _ -> show a <> "n") <> (
    if b < 0
      then show b
      else "+" <> show b)

spacing = "\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;" :: String

exo1 :: forall eq b r.
   { code :: Int | r }
                 -> (String -> State (Array (Html b)) Unit)
                    -> eq -> Rand 
                       -> State (Array (Html b)) Unit
exo1 model m equation r0 = do
  section "Exercice 1"
  t "Soit "
  m "(u_n)"
  t " la suite définie sur "
  m "\\mathbb{N}"
  t " par "
  let {a,b} = randomParam r0
  m $ "u_n = " <> showSeq {a,b}
  t "."
  nl
  nl
  m $ "u_0=" <> spacing <> "u_1=" <> spacing <> "u_2=" <> spacing <> "u_3="

  if model.code < 0
    then do
          nl
          t "réponse: "
          t $ show [b,a+b,2*a+b,3*a+b]
    else pure mempty
