module Main where

import Prelude
import Effect (Effect)
import Data.Maybe(Maybe(..),fromJust)
import Partial.Unsafe(unsafePartial)
import SVG.Geometry (circle, length, point, segment, vector)
import SVG.Render(class Render,render',defaultContext)
import KaTeX (equation, newline, newlineIn, raw, rawIn
             , render, renderIn, setTitle, list, cat, subrender, subraw)
import DOM.Editor as DOM
import Data.Array(replicate, (!!))
import Data.Array(length) as Array
import Data.Foldable(foldr)
import Math(pi,cos,sin)
import Data.Ord(abs)

type Point = { exact :: String
             , approx :: Number
             , cos :: String
             , sin :: String
             , princ :: String}

foreign import fromString :: String -> Int 

aVeryBigPowerOf2 = 33554432 :: Int -- 2^25

type Rand = {val :: Int, gen :: Int}

rand :: Rand -> Rand
rand {val, gen} = 
  let y = (gen * (4*gen + 1) +1) `mod` aVeryBigPowerOf2
  in {val: y `mod` (Array.length points), gen: y} 

m5pi_6 = "-\\frac{5\\pi}{6}" :: String
m3pi_4 = "-\\frac{3\\pi}{4}" :: String
m2pi_3 = "-\\frac{2\\pi}{3}" :: String
mpi_3 = "-\\frac{\\pi}{3}" :: String
mpi_4 = "-\\frac{\\pi}{4}" :: String
mpi_6 = "-\\frac{\\pi}{6}" :: String

pi_6 = "\\frac{\\pi}{6}" :: String
pi_4 = "\\frac{\\pi}{4}" :: String
pi_3 = "\\frac{\\pi}{3}" :: String
tpi_3 = "\\frac{2\\pi}{3}" :: String
tpi_4 = "\\frac{3\\pi}{4}" :: String
fpi_6 = "\\frac{5\\pi}{6}" :: String

-- | mesure principale symetrique par rapport a l'axe des abcisses
symx :: String -> String
symx a = 
  case unit of
    unit | a == m5pi_6 -> fpi_6
         | a == m3pi_4 -> tpi_4
         | a == m2pi_3 -> tpi_3
         | a == mpi_3 -> pi_3
         | a == mpi_4 -> pi_4
         | a == mpi_6 -> pi_6
         | a == pi_6 -> mpi_6
         | a == pi_4 -> mpi_4
         | a == pi_3 -> mpi_3
         | a == tpi_3 -> m2pi_3
         | a == tpi_4 -> m3pi_4
         | a == fpi_6 -> m5pi_6
         | otherwise -> a 
         
-- | mesure principale symetrique par rapport a l'axe des ordonnees
symy :: String -> String
symy a  =
  case unit of
    unit | a == m5pi_6 -> mpi_6
         | a == m3pi_4 -> mpi_4
         | a == m2pi_3 -> mpi_3
         | a == mpi_3 -> m2pi_3
         | a == mpi_4 -> m3pi_4
         | a == mpi_6 -> m5pi_6
         | a == pi_6 -> fpi_6
         | a == pi_4 -> tpi_4
         | a == pi_3 -> tpi_3
         | a == tpi_3 -> pi_3
         | a == tpi_4 -> pi_4
         | a == fpi_6 -> pi_6
         | otherwise -> a 

spi_6 = "\\frac{7\\pi}{6}" :: String
fpi_4 = "\\frac{5\\pi}{4}" :: String
fpi_3 = "\\frac{4\\pi}{3}" :: String
fipi_3 = "\\frac{5\\pi}{3}" :: String
spi_4 = "\\frac{7\\pi}{4}" :: String
epi_6 = "\\frac{11\\pi}{6}" :: String

-- | values of ]-pi;pi] as values of [0;2pi[ 
positive :: String -> String
positive a =
  case unit of
    unit | a == m5pi_6 -> spi_6
         | a == m3pi_4 -> fpi_4
         | a == m2pi_3 -> fpi_3
         | a == mpi_3 -> fipi_3
         | a == mpi_4 -> spi_4
         | a == mpi_6 -> epi_6
         | otherwise -> a

p1_2 = "\\frac{1}{2}" :: String
m1_2 = "-\\frac{1}{2}" :: String
ps2_2 = "\\frac{\\sqrt{2}}{2}" :: String
ms2_2 = "-\\frac{\\sqrt{2}}{2}" :: String
ps3_2 = "\\frac{\\sqrt{3}}{2}" :: String
ms3_2 = "-\\frac{\\sqrt{3}}{2}" :: String

thirds :: Array Point
thirds =  [ 
    {exact: "\\frac{-11\\pi}{3}", approx: -11.0*pi/3.0, cos: p1_2, sin: ps3_2, princ: pi_3}
  , {exact: "\\frac{-10\\pi}{3}", approx: -10.0*pi/3.0, cos: m1_2, sin: ps3_2, princ: tpi_3}
  , {exact: "\\frac{-8\\pi}{3}", approx: -8.0*pi/3.0, cos: m1_2, sin: ms3_2, princ: m2pi_3}
  , {exact: "\\frac{-7\\pi}{3}", approx: -7.0*pi/3.0, cos: p1_2, sin: ms3_2, princ: mpi_3}
  , {exact: "\\frac{-5\\pi}{3}", approx: -5.0*pi/3.0, cos: p1_2, sin: ps3_2, princ: pi_3}
  , {exact: "\\frac{-4\\pi}{3}", approx: -4.0*pi/3.0, cos: m1_2, sin: ps3_2, princ: tpi_3}
  , {exact: fpi_3, approx: 4.0*pi/3.0, cos: m1_2, sin: ms3_2, princ: m2pi_3}
  , {exact: fipi_3, approx: 5.0*pi/3.0, cos: p1_2, sin: ms3_2, princ: mpi_3}
  , {exact: "\\frac{7\\pi}{3}", approx: 7.0*pi/3.0, cos: p1_2, sin: ps3_2, princ: pi_3}
  , {exact: "\\frac{8\\pi}{3}", approx: 8.0*pi/3.0, cos: m1_2, sin: ps3_2, princ: tpi_3}
  , {exact: "\\frac{10\\pi}{3}", approx: 10.0*pi/3.0, cos: m1_2, sin: ms3_2, princ: m2pi_3}
  , {exact: "\\frac{11\\pi}{3}", approx: 11.0*pi/3.0, cos: p1_2, sin: ms3_2, princ: mpi_3}]
  
fourths :: Array Point
fourths = 
  [{exact: "\\frac{-11\\pi}{4}", approx: -11.0*pi/4.0, cos: ms2_2, sin: ms2_2, princ: m3pi_4}
  , {exact: "\\frac{-9\\pi}{4}", approx: -9.0*pi/4.0, cos: ps2_2, sin: ms2_2, princ: mpi_4}
  , {exact: "\\frac{-7\\pi}{4}", approx: -7.0*pi/4.0, cos: ps2_2, sin: ps2_2, princ: pi_4}
  , {exact: "\\frac{-5\\pi}{4}", approx: -5.0*pi/4.0, cos: ms2_2, sin: ps2_2, princ: tpi_4}
  , {exact: fpi_4, approx: 5.0*pi/4.0, cos: ms2_2, sin: ms2_2, princ: m3pi_4}
  , {exact: spi_4, approx: 7.0*pi/4.0, cos: ps2_2, sin: ms2_2, princ: mpi_4}
  , {exact: "\\frac{9\\pi}{4}", approx: 9.0*pi/4.0, cos: ps2_2, sin: ps2_2, princ: pi_4}
  , {exact: "\\frac{11\\pi}{4}", approx: 11.0*pi/4.0, cos: ms2_2, sin: ps2_2, princ: tpi_4}]
 
sixths :: Array Point
sixths = 
  [ {exact: "\\frac{-13\\pi}{6}", approx: -13.0*pi/6.0, cos: ps3_2, sin: m1_2, princ: mpi_6}
  , {exact: "\\frac{-11\\pi}{6}", approx: -11.0*pi/6.0, cos: ps3_2, sin: p1_2, princ: pi_6}
  , {exact: "\\frac{-7\\pi}{6}", approx: -7.0*pi/6.0, cos: ms3_2, sin: p1_2, princ: fpi_6}
  , {exact: "\\frac{-5\\pi}{6}", approx: -5.0*pi/6.0, cos: ms3_2, sin: m1_2, princ: m5pi_6}
  , {exact: fpi_6, approx: 5.0*pi/6.0, cos: ms3_2, sin: p1_2, princ: fpi_6}
  , {exact: spi_6, approx: 7.0*pi/6.0, cos: ms3_2, sin: m1_2, princ: m5pi_6}
  , {exact: epi_6, approx: 11.0*pi/6.0, cos: ps3_2, sin: m1_2, princ: mpi_6}
  , {exact: "\\frac{13\\pi}{6}", approx: 13.0*pi/6.0, cos: ps3_2, sin: p1_2, princ: pi_6}]
  
points :: Array Point
points = thirds <> fourths <> sixths

type Decimal = {decimal :: String, origin :: String, sol :: String}
relation1 = "\\big(\\frac{6}{10}\\big)^2+\\big(\\frac{8}{10}\\big)^2=1" :: String
relation2 = "\\big(\\frac{28}{100}\\big)^2+\\big(\\frac{96}{100}\\big)^2=1" :: String
relation3 = "\\big(\\frac{352}{1000}\\big)^2+\\big(\\frac{936}{1000}\\big)^2=1" :: String
decimals :: Array Decimal
decimals = [ {decimal: "-0.96", origin: relation2, sol: "-0.28"}
           , {decimal: "-0.936", origin: relation3, sol: "-0.352"}
           , {decimal: "-0.8", origin: relation1, sol: "-0.6"}
           , {decimal: "-0.6", origin: relation1, sol: "-0.8"}
           , {decimal: "-0.352", origin: relation3, sol: "-0.936"}
           , {decimal: "-0.28", origin: relation2, sol: "-0.96"} 
           , {decimal: "0.28", origin: relation2, sol: "-0.96"}
           , {decimal: "0.352", origin: relation3, sol: "-0.936"}
           , {decimal: "0.6", origin: relation1, sol: "-0.8"}
           , {decimal: "0.8", origin: relation1, sol: "-0.6"}
           , {decimal: "0.936", origin: relation3, sol: "-0.352"}
           , {decimal: "0.96", origin: relation2, sol: "-0.28"}]

type FromDegree = {degree :: String , radian :: String}
degrees :: Array FromDegree
degrees = [ {degree: "120", radian: tpi_3}
          , {degree: "135", radian: tpi_4}
          , {degree: "150", radian: fpi_6}
          , {degree: "210", radian: spi_6}
          , {degree: "225", radian: fpi_4} 
          , {degree: "240", radian: fpi_3}
          , {degree: "300", radian: fipi_3}
          , {degree: "315", radian: spi_4}
          , {degree: "330", radian: epi_6}]

type FromRadian = FromDegree  
fifths :: Array FromRadian
fifths = [ {radian: "\\frac{2\\pi}{5}", degree: "72"}
         , {radian: "\\frac{3\\pi}{5}", degree: "108"}
         , {radian: "\\frac{4\\pi}{5}", degree: "144"}
         , {radian: "\\frac{6\\pi}{5}", degree: "216"}
         , {radian: "\\frac{7\\pi}{5}", degree: "252"}
         , {radian: "\\frac{8\\pi}{5}", degree: "288"}
         , {radian: "\\frac{9\\pi}{5}", degree: "324"}]

cb :: DOM.Document -> DOM.Event -> Effect Unit
cb doc = unsafePartial $ \ev -> do
  val <- DOM.inputedValueFromEvent ev
  let r0 = rand {val: abs $ fromString val, gen: abs $ fromString val}
  newline
  
  list [ cat [subraw "10 questions"] 
       , cat [subrender "\\frac{1}{2}", subraw " point par bonne réponse"]
       , cat [subraw "sans calculatrice"]] 
  newline
  raw "1) On considère "
  render "\\mathcal{C}"
  raw " le cercle trigonométrique suivant :"
  newline
  div <- DOM.createElement "div" doc
  _ <- DOM.setAttribute "style" "display: grid; grid-template-columns : 1fr 4fr;" div
  thisPosition <- DOM.getElementById "description" doc
  right <- DOM.createElement "div" doc
  svg <- DOM.newSVG "position: relative; width: 100%; height: 100%" div
  _ <- DOM.appendChild right div
  _ <- DOM.appendChild div thisPosition
  let ctx = (defaultContext svg){ strokeWidth = 1.0}
  let draw :: forall a. Render a => a -> Effect Unit
      draw = render' ctx 
  let origx = 80.0
  let origy = 80.0
  let unit = 70.0
  let pO = point "O" origx origy
  let pI = point "I" (origx + unit) origy
  let pJ = point "J" origx (origy - unit)
  draw [pO, pI, pJ]
  draw $ circle pO $ length $ vector pO pI
  draw $ segment pO pI Nothing
  draw $ segment pO pJ Nothing
  let angle0 = fromJust $ points !! r0.val
  let pM = point "M" (origx + unit * cos angle0.approx) (origy - unit * sin angle0.approx)
  draw pM
  draw $ segment pO pM Nothing
  rawIn right "Soit "
  renderIn right "M"
  rawIn right " le point de coordonnées "
  renderIn right $ "(" <> angle0.cos <> "," <> angle0.sin <> ")."
  newlineIn right
  rawIn right "Donner la valeur (en radians) du réel compris dans l'intervalle "
  renderIn right "]-\\pi, \\pi]"
  rawIn right " correspondant au point "
  renderIn right "M:"
  
  newline
  raw "2) Compléter la phrase en remplaçant chaque mot manquant par "
  render "\\underline{égaux}"
  raw " ou "
  render "\\underline{opposés}:"
  let r1 = rand r0
  let arr = case r1.val `mod` 3 of
              0 -> thirds
              1 -> fourths
              otherwise -> sixths
  let n = Array.length arr
  let r2 = rand r1
  let i1 = r2.val `mod` n
  let r3 = rand r2
  let i2 = (i1 + 1 + (r3.val `mod` (n-1))) `mod` n
  let angle1 = fromJust $ arr !! i1
  let angle2 = fromJust $ arr !! i2
  newline
  raw "Les réels "
  render angle1.exact
  raw " et "
  render angle2.exact
  raw " ont leurs cosinus "
  render $ "\\underline{" <> spacex 16 <> "}"
  raw " et leurs sinus "
  render $ "\\underline{" <> spacex 16 <> "}."
  
  newline
  newline
  raw "3) Donner la valeur du réel "
  render "a"
  let r4 = rand r3
  let angle3 = fromJust $ points !! r4.val
  raw " de l'intervalle "
  render "[0, 2\\pi["
  raw " vérifiant "
  newline
  render $ "\\cos(a)=" <> angle3.cos
  raw " et "
  newline
  render $ "\\sin(a)=" <> angle3.sin <> ":"
  newline
  newline
  newline
  
  newline
  raw "4) Etant donné que "
  let r5 = rand r4
  let cosa = fromJust $ decimals !! (r5.val `mod` (Array.length decimals))
  equation cosa.origin
  newline
  raw " et sachant que "
  render "a\\in]-\\pi,0["
  raw ", et que "
  render $ "\\cos(a)=" <> cosa.decimal
  raw ", déterminer la valeur exacte de "
  render "\\sin(a):"
  newline
  newline
  newline
  
  newline
  raw "5) Convertir "
  let r6 = rand r5
  let deg  = fromJust $ degrees !! (r6.val `mod` (Array.length degrees))
  render $ deg.degree <> "^\\circ" 
  raw " en radians. On donnera une réponse dans "
  render "[0, 2\\pi["
  raw " sous la forme exacte la plus simple possible:"
  newline
  newline
  newline
   
  newline 
  raw "6) Convertir "
  let r7 = rand r6
  let fifth = fromJust $ fifths !! (r7.val `mod` (Array.length fifths))
  render fifth.radian
  raw " en degrés. On donnera un entier positif inférieur à "
  render "359^\\circ"
  raw ":"
  newline
  newline
  newline
  
  newline
  raw "7) Soit "
  render "M"
  raw " le point du cercle trigonométrique vérifiant "
  let r8 = rand r7
  let angle4 = fromJust $ points !! r8.val
  render $ "\\widehat{IOM} =" <> angle4.exact
  raw " (remarque: on écrit aussi "
  render $ "(\\overrightarrow{OI},\\overrightarrow{OM})=" <> angle4.exact
  raw "). Donner la valeur de "
  render "\\widehat{IOM'} "
  raw " comprise entre "
  render "-\\pi"
  raw " et "
  render "\\pi"
  raw " si "
  render "M'"
  raw " est le symétrique de "
  render "M"
  raw " par la symétrie d'axe "
  render "(OI):"
  newline
  newline
  newline
  
  newline
  raw "8) Soit "
  render "M"
  raw " le point du cercle trigonométrique vérifiant "
  let r9 = rand r8
  let angle5 = fromJust $ points !! r9.val
  render $ "\\widehat{IOM} =" <> angle5.exact
  raw ". Donner la valeur de "
  render "\\widehat{IOM'}"
  raw " comprise entre "
  render "-\\pi"
  raw " et "
  render "\\pi"
  raw " si "
  render "M'"
  raw " est le symétrique de "
  render "M"
  raw " par la symétrie d'axe "
  render "(OJ):"
  newline
  newline
  newline
  
  newline
  raw "9) Donner la valeur de "
  render $ "\\cos(" <> angle4.exact <> "):"
  newline
  newline
  newline
  
  newline
  raw "10) Donner la valeur de "
  render $ "\\sin(" <> angle5.exact <> "):"
  newline
  newline
  newline
    
  let rep = ["réponses: 1)",angle0.princ
            ,"\\;2)",(if angle1.cos == angle2.cos then "égaux," else "opposés,") <>
                  (if angle1.sin == angle2.sin then "égaux" else "opposés")
            ,"\\; 3)", positive angle3.princ
            ,"\\; 4)", cosa.sol
            ,"\\; 5)", deg.radian
            ,"\\; 6)", fifth.degree <> "^\\circ"
            ,"\\; 7)", symx angle4.princ
            ,"\\; 8)", symy angle5.princ
            ,"\\; 9)", angle4.cos
            ,"\\; 10)", angle5.sin]
  render $ if fromString val < 0 then foldr (<>) "" rep else ""
  
  
spacex :: Int -> String 
spacex n = foldr (<>) "" $ replicate n "\\;"

main :: Effect Unit
main = void $ unsafePartial do
  setup <- DOM.setup
  
  seed <- DOM.createElement "input" setup.document
  _ <- DOM.addEventListener (cb setup.document) DOM.change seed
  _ <- DOM.appendChild seed setup.body
  setTitle "Devoir 1 : Trigonométrie"
  raw "Nom:"
  render $ spacex 40
  raw "Prénom:"
  render $ spacex 40
  raw "Classe:"
  pure unit
  
