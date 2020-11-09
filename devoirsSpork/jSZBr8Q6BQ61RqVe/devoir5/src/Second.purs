module Second where

import Prelude
import Data.Maybe(fromJust)
import Data.Array((!!),length,take)
import DOM.Editor as DOM
import Effect(Effect)
import KaTeX (newline, raw, render
             , bold, emph)
import Rand(Rand, unsort)
import Partial.Unsafe(unsafePartial)
import Data.FoldableWithIndex(forWithIndex_, foldMapWithIndex)

problems :: Array {q :: String
                  , a :: String
                  , b :: String
                  , c :: String
                  , r :: String}
problems = [{q: "La probabilité que le lecteur choisisse un roman policier est", a: "0,4", b: "0,75", c: "\\dfrac{1}{7}", r: "b"}
           ,{q: "La probabilité que le lecteur choisisse une biographie est", a: "\\dfrac{1}{4}", b: "\\dfrac{6}{7}", c: "0,6", r: "a"}
           ,{q: "Le lecteur ayant choisi un roman policier, la probabilité que l’auteur soit français est", a: "0,3", b: "0,8", c: "0,4", r: "c"}
           ,{q: "Le lecteur ayant choisi une biographie, la probabilité que l’auteur soit français est", a: "0,7", b: "0,5", c: "0,2", r: "a"}
           ,{q: "La probabilité que le lecteur choisisse un roman policier français est", a: "1,15", b: "0,4", c: "0,3", r: "c"}
           ,{q: "La probabilité que le lecteur choisisse une biographie française est", a: "0,15", b: "0,175", c: "0,125", r: "b"}
           ,{q: "La probabilité que le lecteur choisisse un livre d’un écrivain français est", a: "0,9", b: "0,7", c: "0,475", r: "c"}
           ,{q: "La probabilité que le lecteur ait choisi un roman policier sachant que l’écrivain est français est", a: "\\dfrac{3}{7}", b: "\\dfrac{12}{19}", c: "0,3", r: "b"}
           ,{q: "La probabilité que le lecteur ait choisi une biographie sachant que l’écrivain est français est", a: "\\dfrac{7}{19}", b: "\\dfrac{4}{7}", c: "0,7", r: "a"}
           ,{q: "Le choix d'un roman policier et le choix d'un livre d'un auteur français ne sont pas indépendants. Si on voulait rendre ces choix indépendants, le nombre de romans policiers d'écrivains français à ajouter au stock serait", a: "200", b: "105", c: "45", r: "a"}
           ,{q: "La probabilité que lecteur choisisse une biographie ou un livre d'un auteur français est", a: "\\dfrac{19}{40}", b: "\\dfrac{11}{20}", c: "0.375", r: "b"}
           ]
           

exo1 :: Rand -> DOM.Document -> Boolean -> Effect Unit
exo1 r doc mode = do
  emph "Cet exercice est un questionnaire à choix multiple. Pour chaque question, une seule des réponses proposées est correcte. Une bonne réponse rapporte 1 point, une mauvaise réponse enlève 1 point tant que la note globale reste positive. Une absence de réponse n'apporte et n'enlève aucun point. Aucune justification n'est exigée."
  newline
  raw "Un lecteur d'une bibliothèque est passionné de romans policiers et de biographies. Cette bibliothèque lui propose 150 romans policiers et 50 biographies."
  newline
  raw "40% des écrivains de romans policiers sont français et 70% des écrivains de biographies sont français."
  newline
  raw "Le lecteur choisit un livre au hasard parmi les 200 ouvrages."
  newline
  newline
  let chosen_indices = unsort (length problems) r
  let chosen_problems = 
         take 5 $ (\i -> unsafePartial $ fromJust 
                                       $ problems !! i) <$> chosen_indices
  forWithIndex_ chosen_problems (\i p -> do
                    bold $ (show $ i+1) <> ". "
                    raw $ p.q <> " :"
                    newline
                    newline
                    raw "(a) "
                    render p.a
                    render "\\quad\\quad"
                    raw "(b) "
                    render p.b
                    render "\\quad\\quad"
                    raw "(c) " 
                    render p.c
                    newline
                    newline)
  if mode 
    then raw $ "réponses: " <> foldMapWithIndex (\i a -> " " <> show (i+1) <> ") ("<> a.r <> ")") chosen_problems
    else pure unit
   
second :: Rand -> DOM.Document -> Boolean -> Effect Unit
second = exo1
