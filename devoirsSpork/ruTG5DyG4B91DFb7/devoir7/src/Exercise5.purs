module Exercise5 where

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
    -1 -> "-n^2-t_n"
    1 -> "n^2-t_n"
    _ -> show a <> "n^2-t_n")

spacing = "\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;\\;" :: String

exo5 :: forall eq b r.
   { code :: Int | r }
                 -> (String -> State (Array (Html b)) Unit)
                    -> eq -> Rand 
                       -> State (Array (Html b)) Unit
exo5 model m equation r0 = do
  section "Exercice 5"
  t "Soit "
  m "(t_n)"
  t " la suite définie sur "
  m "\\mathbb{N}"
  t " par "
  let {a,b} = randomParam r0
  m $ "\\left\\{\\begin{array}{l}t_0=" <> show b <> "\\\\"
                                 <> "t_{n+1}=" <> showSeq {a,b}
                                 <> "\\end{array}\\right."
  t "."
  nl
  nl
  m $ "t_0=" <> spacing <> "t_1=" <> spacing <> "t_2=" <> spacing <> "t_3="

  if model.code < 0
    then do
          nl
          t "réponse: "
          t $ show [b,-b,a+b,3*a-b]
    else pure mempty
