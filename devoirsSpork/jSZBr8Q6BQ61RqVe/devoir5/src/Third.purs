module Third where

import Prelude
import Data.Maybe(fromJust)
import Data.Array((!!),length,take)
import DOM.Editor as DOM
import Effect(Effect)
import KaTeX ( newline, raw, render
             , bold, equation)
import Rand(Rand, unsort)
import Partial.Unsafe(unsafePartial)
import Data.FoldableWithIndex(forWithIndex_, foldMapWithIndex)

problems :: Array { domain :: String
                  , inequation :: String
                  , answer :: String}
problems = [{ domain: "\\mathbb{R} \\backslash \\{1;2\\}"
            , inequation: "\\dfrac{2}{x-1}-\\dfrac{1}{x-2}<3"
            , answer: "]-\\infty; 1[\\cup]2;+\\infty["
            }
           ,{ domain: "\\mathbb{R} \\backslash \\{0;4\\}"
            , inequation: "\\dfrac{3x-2}{4-x}\\geq \\dfrac{1}{x}"
            , answer: "[-1;0[\\cup[\\dfrac{4}{3};4["
            }
           ,{ domain: "\\mathbb{R} \\backslash \\{-1;1\\}"
            , inequation: "\\dfrac{x}{x^2-1}\\leq \\dfrac{1}{1-x^2}"
            , answer: "]-\\infty;-1[\\cup]-1;1["
            }
           ] 

exo1 :: Rand -> DOM.Document -> Boolean -> Effect Unit
exo1 r doc mode = pure unit

exo2 :: Rand -> DOM.Document -> Boolean -> Effect Unit
exo2 r doc mode = pure unit

third :: Rand -> DOM.Document -> Boolean -> Effect Unit
third r doc mode = 
  do
        let chosen_indices = unsort (length problems) r
        let chosen_problems = 
              take 2 $ (\i -> 
                   unsafePartial $ fromJust 
                                 $ problems !! i) <$> chosen_indices
        forWithIndex_ chosen_problems (\i p -> do
            bold $ (show $ i+1) <> "••◦"
            raw " Résoudre dans "
            render p.domain
            raw " :"
            newline
            equation p.inequation)
        
        if mode 
         then render $ "réponses: " <> foldMapWithIndex (\i p -> 
                " " <> show (i+1) 
                    <> ") " 
                    <> p.answer 
                    <> "\\quad") chosen_problems
         else pure unit
