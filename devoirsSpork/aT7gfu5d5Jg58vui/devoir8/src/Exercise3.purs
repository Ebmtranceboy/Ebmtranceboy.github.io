module Exercise3 where

import Prelude

import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Spork.Html (Html)
import Data.Array ((!!), length, take)
import Data.FoldableWithIndex (forWithIndex_, foldMapWithIndex)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Rand (unsort)
import SporKaTeX (b, nl, section, t)

problems :: Array { domain :: String
                  , sequence :: String
                  , answer :: String}
problems = [{ domain: "n\\in\\mathbb{N}"
            , sequence: "u_n=\\dfrac{2n+1}{n+3}"
            , answer: "strictement croissante pour tout n"
            }
           ,{ domain: "n\\in\\mathbb{N}"
            , sequence: "u_n=2n^2-3n-2"
            , answer: "décroissante sur {0,1}, croissante sur pour n>=1"
            }
           ,{ domain: "n\\in\\mathbb{N}"
            , sequence: "u_n=1+\\dfrac{1}{n+1}"
            , answer: "décroissante pour tout n"
            }
           ,{ domain: "n\\in\\mathbb{N}"
            , sequence: "u_n=\\dfrac{3n-1}{2-5n}"
             , answer: "décroissante sur {0,1} puis croissante pour n>=1"
            }
             ,{ domain: "n\\geq 1"
              , sequence: "u_n=\\dfrac{2^n}{n}"
               , answer: "croissante pour n>=1"
              }
              ]

exo3 :: forall t3 t8 t84 t92.
  Discard t84 => { code :: Int
                 | t92
                 }
                 -> (String -> StateT (Array (Html t8)) Identity t84)
                    -> t3
                       -> { gen :: Int
                          , seed :: Int
                          , val :: Int
                          }
                          -> StateT (Array (Html t8)) Identity Unit
exo3 model m equation r0 = do
        section "Exercice 3"
        let chosen_indices = unsort (length problems) r0
        let chosen_problems =
              take 2 $ (\i ->
                   unsafePartial $ fromJust
                                 $ problems !! i) <$> chosen_indices
        forWithIndex_ chosen_problems (\i p -> do
            b $ (show $ i+1)
            m "\\bullet\\bullet\\circ\\;"
            t "Soit "
            _ <- m "(u_n)"
            t " la suite définie par "
            _ <- m p.sequence
            t " pour tout "
            _ <- m p.domain
            t "."
            nl
            t " Etudier les variations de "
            _ <- m "(u_n)"
            t "."
            nl)

        if model.code < 0
         then t $ "réponses: " <> foldMapWithIndex (\i p ->
                " " <> show (i+1)
                    <> ") "
                    <> p.answer) chosen_problems
         else pure unit
