module First where

import Prelude
import Data.Maybe(Maybe(..), fromJust)
import Data.Array((!!),length)
import DOM.Editor as DOM
import Effect(Effect)
import KaTeX (cat, list, newline, raw, render
             , subraw, bold, emph, subrender)
import SVG.Geometry (segment,point)
import SVG.Render as SVG
import Rand(Rand)
import Partial.Unsafe(unsafePartial)

tree :: DOM.Document -> Boolean -> Effect Unit
tree doc mode = do
-- https://www.sitepoint.com/how-to-translate-from-dom-to-svg-coordinates-and-back-again/
  thisPosition <- DOM.getElementById "description" doc
  svg <- DOM.newSVG "position: absolute; width: 500px; height: 400px;" thisPosition
  let root = point "" 12.0 175.0
  let nodeA = point "" 100.0 92.0
  let nodeA' = point "" 100.0 244.0
  let startA = point "" 134.0 80.0
  let endAB = point "" 202.0 24.0
  let endAB' = point "" 202.0 136.0
  let startA' = point "" 134.0 264.0
  let endA'B = point "" 202.0 208.0
  let endA'B' = point "" 202.0 320.0

  let frameTL = point "" 0.0 0.0
  let frameTR = point "" 490.0 0.0
  let frameBR = point "" 490.0 350.0
  let frameBL = point "" 0.0 350.0
  let context = { svg
                , stroke: "#000"
                , strokeWidth: 1.5
                , fill: "#00000000"
                , fontStyle: "italic 15px arial, sans-serif"
                , textFill: "#000"}

  SVG.render' context $ segment root nodeA Nothing
  SVG.render' context $ segment root nodeA' Nothing 
  SVG.render' context $ segment startA endAB  Nothing
  SVG.render' context $ segment startA endAB'  Nothing
  SVG.render' context $ segment startA' endA'B  Nothing
  SVG.render' context $ segment startA' endA'B'  Nothing
  
  let pF= if mode then "0.52" else "\\dots"
  let pnF= if mode then "0.48" else "\\dots"
  let pgnFS= if mode then "0.175" else "\\dots"
  let pgnFnS= if mode then "0.825" else "\\dots"
  
  render $ "\\begin{array}{ccccccccc} & & & & & & & & S \\\\ "
          <> "& & & & & " <>  " \\\\ \\\\ "
          <> "& & & & F \\\\ "
          <> "& " <> pF <> " \\\\ "
          <> "& & & & & "  <> " \\\\ "
          <> "& & & & & & & &" <> "\\overline{S} \\\\ "
          <> "\\cdot \\\\ "
          <> "& & & & & & & &" <> "S \\\\ "
          <> "& & & & &" <> pgnFS <>" \\\\ "
          <> "&" <> pnF <> " \\\\ "
          <> "& & & & \\overline{F} \\\\ \\\\"
          <> "& & & & & " <> pgnFnS <> " \\\\ "
          <> "& & & & & & & & \\overline{S} \\end{array}"



exo1 :: Rand -> DOM.Document -> Boolean -> Effect Unit
exo1 _ doc mode = do
  emph "Dans cet exercice, on arrondira les résultats au millième."
  newline
  raw "Une agence Pôle Emploi étudie l'ensemble des demandeurs d'emploi selon deux critères, le sexe et l'expérience professionnelle."
  newline
  raw "Cette étude montre que :"
  newline
  list [ cat [subraw "52% des demandeurs d'emploi sont des femmes et 48% sont des hommes ;"]
       , cat [subraw "18% des demandeurs d'emploi sont sans expérience et les autres sont avec expérience ;"]
       , cat [subraw "parmi les hommes qui sont demandeurs d'emploi, on sait que 17,5% sont sans expérience."]
       ]
  newline
  raw "On prélève au hasard la fiche d'un demandeur d'emploi de cette agence. On note:"
  newline
  list [ cat [subrender "S",  subraw " : l'événement \"le demandeur d'emploi est sans expérience\" ;"]
       , cat [subrender "F", subraw " : l'événement \"le demandeur d'emploi est une femme\"."]
       ]
  
  newline
  bold "1•"
  raw " Préciser "
  render "p(S)"
  raw " et "
  render "p_{\\overline{F}}(S)"
  raw "."
  
  newline
  bold "2•"
  raw " Recopier l'arbre ci-dessous et compléter les pointillés par les probabilités associées."
  newline
  tree doc mode
  
  newline
  bold "3•"
  raw " Démontrer que "
  render "p(\\overline{F}\\cap S)=0,084"
  raw ". Interpréter le résultat."
  
  newline
  bold "4•"
  raw " La fiche prélevée est celle d'un demandeur d'emploi sans expérience. Calculer la probabilité pour que ce soit un homme."
  
  newline
  bold "5•"
  newline
  render "\\quad"
  bold " a)"
  raw " Justifier que "
  render "p(S\\cap F)=0,096."
  
  newline
  render "\\quad"
  bold " b)"
  raw " Sachant que la fiche prélevée est celle d'une femme, calculer la probabilité que ce soit la fiche d'un demandeur d'emploi sans expérience."
  
  if mode
    then do
       newline
       raw "réponses: 1) p(S)=0.18 pnF(S)=0.175 2)3)4) pS(nF)~0.467 5)a)b) pF(S)~0.185"
    else pure unit

exo2 :: Rand -> DOM.Document -> Boolean -> Effect Unit
exo2 _ doc mode = do
  emph "Dans cet exercice, on arrondira les résultats au millième."
  newline
  raw "Une entreprise spécialisée dans la fabrication de confitures fait appel à des producteurs locaux."
  newline
  raw "À la livraison, l’entreprise effectue un contrôle qualité à l’issue duquel les fruits sont sélectionnés ou non pour la préparation des confitures."
  newline
  raw "Une étude statistique a établi que :"
  list [ cat [subraw "22% des fruits livrés sont issus de l’agriculture biologique ;"]
       , cat [subraw "parmi les fruits issus de l’agriculture biologique, 95% sont sélectionnés pour la préparation des confitures ;"]
       , cat [subraw "parmi les fruits non issus de l’agriculture biologique, 90% sont sélectionnés pour la préparation des confitures."]
       ]
  raw "On prélève au hasard un fruit et on note :"
  list [cat [subrender "B", subraw " l'événement \"le fruit est issu de l’agriculture biologique\" ;"]
       ,cat [subrender "S", subraw " l'événement \"le fruit est sélectionné pour la préparation des confitures\"."]
       ]
  bold "1•"
  raw " Représenter la situation par un arbre pondéré."
  
  newline
  bold "2•◦"
  newline
  render "\\quad"
  bold "a)"
  raw " Calculer "
  render "p(B\\cap S)."
  newline
  render "\\quad"
  bold "b)"
  raw " Interpréter ce résultat dans le cadre de l'énoncé."
  
  newline
  bold "3•◦"
  raw " Montrer que la probabilité que le fruit est selectionné pour la préparation des confitures vaut 0,911."
  
  newline 
  bold "4•"
  raw " Sachant que le fruit a été selectionné pour la préparation des confitures, déterminer la probabilité qu'il ne soit pas issu de l'agriculture biologique."
  if mode
    then do
       newline
       raw "réponses: 1)2)a)p(B/\\S)=0.209 b)3)pS(nB)~0.771"
    else pure unit

first :: Rand -> DOM.Document -> Boolean -> Effect Unit
first r doc mode = 
  let arr = [exo1 r doc mode, exo2 r doc mode]
  in unsafePartial $ fromJust 
                   $ arr !! r.val `mod` (length arr)

