module Exercise4 where

import Prelude

import Control.Monad.State (StateT)
import Data.Identity (Identity)
import SporKaTeX (t, nl, section, b)
import Spork.Html (Html)

exo4 :: forall t35 t40 t78 t85.
  Discard t78 => { code :: Int
                 | t85
                 }
                 -> (String -> StateT (Array (Html t40)) Identity t78)
                    -> t35
                       -> { gen :: Int
                          , seed :: Int
                          , val :: Int
                          }
                          -> StateT (Array (Html t40)) Identity Unit
exo4 model m equation r0 = do
  section "Exercice 4"
  t "Soit "
  m "(p_n)"
  t " la suite définie pour "
  m "n\\geq 2"
  t " par "
  m $ "\\left\\{\\begin{array}{l}p_2=\\sqrt{6}\\\\"
                                 <> "p_{n+1}=\\sqrt{p_n^2+\\dfrac{6}{n^2}}"
                                 <> "\\end{array}\\right."
  t "."
  nl
  nl

  b "1"
  m "\\bullet\\bullet\\;"
  t "Montrer que "
  m "p_n>0"
  t " pour tout "
  m "n\\geq 2"
  t "."
  nl

  b "2"
  m "\\bullet\\bullet\\;"
  t "Montrer que "
  m "(p_n)"
  t " est croissante pour tout "
  m "n\\geq 2"
  t "."
  nl

  b "3"
  m "\\bullet\\;"
  t "A l'aide de la calculatrice, dire si la suite semble convergente."
  nl
  t "Si oui, conjecturer la valeur de sa limite quand "
  m "n"
  t " tend vers "
  m "+\\infty"
  t "."
  nl
  nl
