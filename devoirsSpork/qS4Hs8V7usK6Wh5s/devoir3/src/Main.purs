module Main where

import Prelude
import Effect (Effect)
import Data.Maybe(Maybe(..),fromJust)
import Partial.Unsafe(unsafePartial)
import KaTeX (cat, equation, list, newline, newlineIn, raw, rawIn, render, renderIn, setTitle, section, subsection, subraw, subrender)
import DOM.Editor as DOM
import Data.Array(replicate, (!!), (\\), scanl, last)
import Data.Array(length) as Array
import Data.Foldable(foldr)
import Data.Ord(abs) as Ord
import Fraction (Fraction(..), inlineFraction, fromInt, abs)
import Rand (Rand, rand)
import SVG.Geometry (point, segment, middle, meets,line,rename)
import SVG.Render(class Render,render',defaultContext)
import Math(pi)

foreign import fromString :: String -> Int 

primes :: Array Int
primes = [2,2,2,2,2,2,3,3,3,3,3,5,5,5,5]

avgNbFactors = 2 :: Int

randFraction :: Rand -> {fraction :: Fraction, nextRand :: Rand}
randFraction = unsafePartial \ r -> 
  let r0 = rand r
      nbNumFactors = 1 + (r0.val `mod` avgNbFactors)
      r1 = rand r0
      nbDenFactors = 1 + (r1.val `mod` avgNbFactors)
      nrands = (\f -> f r1) <$> (scanl (<<<) identity 
                             $ replicate nbNumFactors rand)
      Just r2 = last nrands
      drands = (\f -> f r2) <$> (scanl (<<<) identity 
                             $ replicate nbDenFactors rand)
      Just r3 = last drands
      nextRand = rand r3
      sign = 2 * (nextRand.val `mod` 2) - 1
      prime = unsafePartial $ \ ix -> fromJust $ primes !! ix 
      nums = prime <$> (\rnd -> rnd.val `mod` (Array.length primes)) 
                   <$> nrands
      dens = prime <$> (\rnd -> rnd.val `mod` (Array.length primes)) 
                   <$> drands
      num = foldr (*) 1 $ nums \\ dens
      den = foldr (*) 1 $ dens \\ nums
   in if num > den 
        then {fraction: Fraction {num: sign * num, den}, nextRand}
        else {fraction: Fraction {num: sign * den, den: num}, nextRand}

alpha :: Fraction -> Fraction -> Fraction -> Fraction
alpha a b c = -b/a/fromInt 2

beta :: Fraction -> Fraction -> Fraction -> Fraction
beta a b c = 
  let al = alpha a b c
      f x = a * x * x + b * x + c
   in f al

delta :: Fraction -> Fraction -> Fraction -> Fraction
delta a b c = b * b - fromInt 4 * a * c

showTrinom :: Fraction -> Fraction -> Fraction -> String
showTrinom a b c =
  let m2 = 
        case unit of
             unit | a == - one -> "-x^2"
                  | a == one -> "x^2"
                  | otherwise -> show a <> "x^2"
      m1 =
        case unit of
             unit | b == - one -> "-x"
                  | b < zero -> "-" <> (show $ abs b) <> "x"
                  | b == zero -> ""
                  | b == one -> "+x"
                  | otherwise -> "+" <> show b <> "x"
   in m2 <> m1 <> inlineFraction c 

type Point = { exact :: String
             , approx :: Number
             , cos :: String
             , sin :: String
             , princ :: String}

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

pythagoreanTriples :: Array {a :: Int, b:: Int, c :: Int}
pythagoreanTriples = [ {a: 3, b: 4, c: 5}
                     , {a: 20, b: 21, c: 29}
                     , {a: 11, b: 60, c: 61}
                     , {a: 13, b: 84, c: 85}
                     , {a: 5, b: 12, c: 13}
                     , {a: 12, b: 35, c: 37}
                     , {a: 16, b: 63, c: 65}
                     , {a: 36, b: 77, c: 85}
                     , {a: 8, b: 15, c: 17}
                     , {a: 9, b: 40,c: 41}
                     , {a: 33, b: 56, c: 65}
                     , {a: 39, b: 80,c: 89}
                     , {a: 7, b: 24, c: 25}
                     , {a: 28, b: 45, c: 53}
                     , {a: 48, b: 55, c: 73}
                     , {a: 65, b: 72, c: 97}
                     ]



cb :: DOM.Document -> DOM.Event -> Effect Unit
cb doc = unsafePartial $ \ev -> do
  val <- DOM.inputedValueFromEvent ev
  let odd = 2 * (Ord.abs $ fromString val) + 1
  let r0 = {val: odd, gen: 0, seed: odd * odd}
  newline
  
  list [ cat [subraw "4 exercices"] 
       , cat [subraw "5 points par exercice "
             , subrender "(\\bullet"
             , subraw ": 1 point, "
             , subrender "\\circ : \\frac{1}{2}"
             , subraw " point)"]
       , cat [ subraw "qualité de la rédaction "
             , subraw "prise en compte"]
       , cat [subraw "sans document"]
       , cat [subraw "calculatrice autorisée"]]
  
  newline
  thisPosition <- DOM.getElementById "description" doc
  
  section "Exercice 1"
  div1 <- DOM.createElement "div" doc
  _ <- DOM.setAttribute "style" "display: grid; grid-template-columns : 2fr 4fr;" div1
  right1 <- DOM.createElement "div" doc
  svg <- DOM.newSVG "position: relative; width: 400; height: 310;" div1
  let ctx = (defaultContext  svg){ strokeWidth = 1.0}
  _ <- DOM.appendChild right1 div1
  _ <- DOM.appendChild div1 thisPosition
  let draw1 :: forall a. Render a => a -> Effect Unit
      draw1 = render' ctx 
  let origx1 = 150.0
  let origy1 = 90.0
  let theUnit1 = 55.0
  let x' = 25.0
  let pO = point "O" (origx1)  (origy1)
  let pA = point "A" (origx1 - 2.0 * theUnit1)  (origy1 + theUnit1)
  let pB = point "B" (origx1 + 2.0 * theUnit1) (origy1 + theUnit1)
  let pC = point "C" (origx1 + 2.0 * theUnit1) (origy1 - theUnit1)
  let pD = point "D" (origx1 - 2.0 * theUnit1) (origy1 - theUnit1)
  let pE = point "" (origx1 - 2.0 * theUnit1 + x')  (origy1 + theUnit1 - x')
  let pF = point "" (origx1 + 2.0 * theUnit1 - x') (origy1 + theUnit1 - x')
  let pG = point "" (origx1 + 2.0 * theUnit1 - x') (origy1 - theUnit1 + x')
  let pH = point "" (origx1 - 2.0 * theUnit1 + x') (origy1 - theUnit1 + x')
  draw1 [pO, pA, pB, pC, pD]
  draw1 [segment pA pB Nothing, segment pB pC Nothing, segment pC pD Nothing, segment pD pA Nothing]
  draw1 [segment pE pF Nothing, segment pF pG Nothing, segment pG pH Nothing, segment pH pE Nothing]
  let pI = middle "" $ segment pA pD Nothing
  let [pK] = (line pO pI) `meets` (line pE pH)
  draw1 [ rename "𝑥" pI, pK]
  draw1 $ segment pI pK Nothing
  
  let pJ = middle "" $ segment pB pC Nothing
  let [pL] = (line pO pJ) `meets` (line pF pG)
  draw1 [ rename "𝑥" pL, pJ]
  draw1 $ segment pJ pL Nothing
  
  let pM = middle "" $ segment pA pB Nothing
  let [pN] = (line pO pM) `meets` (line pE pF)
  draw1 [ rename "𝑥" pM, pN]
  draw1 $ segment pM pN Nothing
  
  let pP = middle "" $ segment pC pD Nothing
  let [pQ] = (line pO pP) `meets` (line pG pH)
  draw1 [ rename "𝑥" pQ, pP]
  draw1 $ segment pQ pP Nothing
  
  raw "On donne "
  let r1 = rand r0
  let triple = fromJust $ pythagoreanTriples !! (r1.val `mod` Array.length pythagoreanTriples)
  let ab = triple.b
  let ad = triple.a
  render $ " AB=" <> show ab
  raw " et "
  render $ "AD=" <> show ad
  raw " (la figure n'est pas à l'échelle)."
  newlineIn right1 
  newlineIn right1 
  
  rawIn right1 "Soit "
  renderIn right1 "ABCD"
  rawIn right1 " un rectangle de centre "
  renderIn right1 "O"
  rawIn right1 "."
  newlineIn right1 
  rawIn right1 "On construit un autre rectangle de centre "
  renderIn right1 "O"
  rawIn right1 " à l'intérieur de "
  renderIn right1 "ABCD"
  rawIn right1 " de manière à laisser entre les deux rectangles "
  rawIn right1 "une bande de largeur constante "
  renderIn right1 "x"
  rawIn right1 "."
  newlineIn right1
  newlineIn right1
  rawIn right1 "Le but de l'exercice est de trouver la valeur de "
  renderIn right1 "x"
  rawIn right1 " pour laquelle "
  rawIn right1 " l'aire du rectangle intérieur est égale à la moitié "
  rawIn right1 " de l'aire de "
  renderIn right1 "ABCD"
  rawIn right1 "."
  newlineIn right1
  newlineIn right1
 
  subsection "1◦"
  raw "Expliquer brièvement pourquoi le rectangle intérieur a pour longeur "
  render $ show ab <> "-2x"
  raw " et pour hauteur "
  render $ show ad <> "-2x"
  raw "."
  newline
  raw "Ceci implique que "
  render "x"
  raw " ne peut dépasser une valeur maximale. Laquelle ? Expliquer brièvement."
  
  subsection "2•◦"
  raw "Donner l'aire de "
  render "ABCD."
  newline
  raw "Montrer que l'aire du rectangle intérieur est "
  render $ "4x^2 -" <> show (2*(ab+ad)) <> "x+" <> show (ab*ad)
  raw "."
  newline
  raw "En déduire que l'équation que doit vérifier "
  render "x"
  raw " est"
  equation $ "4x^2 -" <> show (2*(ab+ad)) <> "x+" <> show ((ab*ad) `div` 2) <> "=0"

  subsection "3••◦"
  raw "Résoudre cette équation dans "
  render "\\mathbb{R}"
  raw "."
  
  subsection "4◦"
  raw "En prenant en compte les contraintes sur "
  render "x"
  raw " établies à la question 1, donner la valeur de "
  render "x"
  raw " répondant au problème."
  newline
  newline
  
  section "Exercice 2"
  raw "Dans un repère orthonormé, soient "
  render "J"
  raw " le point de coordonnées "
  render "(0;1)"
  raw ", "
  newline
  render "K"
  raw " le point de coordonnées "
  render "(\\cos(\\frac{-5\\pi}{6}); \\sin(\\frac{-5\\pi}{6}))"
  raw " et "
  newline
  render "L"
  raw " le point de coordonnées "
  render "(\\cos(\\frac{-\\pi}{6}); \\sin(\\frac{-\\pi}{6}))"
  raw "."
  
  subsection "1•••◦"
  raw "Calculer les distances "
  render "JK"
  raw ", "
  render "KL"
  raw " et "
  render "LJ"
  raw "."
  newline
  raw "Pour rappel, la distance entre "
  render "A" 
  raw " et "
  render "B"
  raw " est "
  render "\\sqrt{(x_B-x_A)^2+(y_B-y_A)^2}"
  raw " dans un repère orthonormé."
  
  subsection "2•◦"
  raw "En déduire la nature du triangle "
  render "JKL"
  raw ","
  newline
  raw " puis une mesure en radians de l'angle "
  render "\\widehat{JKL}"
  raw "."
  newline
  newline
  
  section "Exercice 3"
  raw "Le but de cet exercice est de résoudre le système en "
  render "x"
  raw " et "
  render "y"
  raw " suivant "
  let f1 = randFraction r1
  let f2 = randFraction f1.nextRand 
  newline
  newline
  equation $ "\\left\\{\\begin{array}{l}x-y=" 
                    <> (show $ f1.fraction - f2.fraction) 
                    <> "\\\\ xy = " 
                    <> (show $ f1.fraction * f2.fraction)
                    <> "\\end{array}\\right."
  newline
  subsection "1◦"
  raw "En effectuant les changements de variables "
  render "X=x"
  raw " et "
  render "Y=-y"
  raw ", montrer que le système initial devient"
  newline
  newline
  equation $ "\\left\\{\\begin{array}{l}X+Y=" 
                    <> (show $ f1.fraction - f2.fraction) 
                    <> "\\\\ XY = " 
                    <> (show $ - f1.fraction * f2.fraction)
                    <> "\\end{array}\\right."
  
  subsection "2•••◦"
  raw "Résoudre le système de la question 1, c'est-à-dire "
  newline
  raw "trouver tous les couples "
  render "(X;Y)"
  raw " solutions du système avec la méthode du cours."
  
  subsection "3•"
  raw "En déduire l'ensemble des couples "
  render "(x;y)"
  raw " solutions du système initial en utilisant à nouveau les changements de variables."
  newline
  newline
  
  section "Exercice 4"
  raw "Dire si chacune des affirmations suivantes est vraie ou fausse."
  newline
  raw "Si elle est vraie, justifier brièvement pourquoi par une propriété du cours, ou un argument géométrique."
  newline
  raw "Sinon, donner un contre-exemple."
  newline
  raw "Toutes les questions sont indépendantes les unes des autres."
  
  subsection "1•◦"
  raw "Soient "
  render "S"
  raw " et "
  render "P"
  raw " deux nombres réels. Il est toujours possible de trouver "
  newline
  raw "deux nombres réels "
  render "x_1"
  raw " et "
  render "x_2"
  raw " tels que "
  render "x_1+x_2=S"
  raw " et "
  render "x_1x_2=P"
  raw "."
  
  subsection "2◦"
  raw "Quelque soient "
  render "x"
  raw " et "
  render "y"
  raw ", "
  render "\\cos(x)+\\sin(y)=\\cos(y)+\\sin(x)"
  raw "."
  
  subsection "3•◦"
  raw "Soit "
  render "f"
  raw " la fonction définie par "
  render "f(x)=a(x-\\alpha)^2+\\beta"
  raw "."
  newline
  raw "Si "
  render "a" 
  raw " et "
  render "\\beta"
  raw " sont de signes contraires, alors "
  render "f"
  raw " admet deux racines réelles distinctes."
  
  subsection "4◦"
  raw "Quelque soit "
  render "x"
  raw ", "
  render "\\cos^2(x)-\\sin^2(x)=1"
  raw "."
  
  subsection "5◦"
  raw "Soit "
  render "f"
  raw " une fonction polynôme du second degré. "
  newline
  raw "Si "
  render "f"
  raw " possède la racine "
  render "x_1"
  raw ", alors elle possède aussi la racine "
  render "x_2"
  raw " et "
  render "x_2\\not = x_1"
  raw "."
   
  subsection "6◦"
  raw "Quelque soient "
  render "x"
  raw " et "
  render "y"
  raw ", "
  render "\\cos(x)\\sin(y)=\\cos(y)\\sin(x)"
  raw "."

  newline
  let rep = ["réponses: "
      ,"\\; 1)1◦ \\; 0<x<"<> show (fromInt ad / fromInt 2)
      ,"\\; 1)2•◦ \\;", "AB AD = "<> show (ab*ad)
      ,"\\; 1)3••◦ \\;", "\\mathcal{S}=\\{" <> show (fromInt (ab+ad-triple.c) 
/ fromInt 4) <> "," <> show (fromInt (ab+ad+triple.c) / fromInt 4) <> "\\} "
      ,"\\; 1)4◦"
      ,"\\; 2)1•••◦ \\;JK=KL=LJ=\\sqrt{3}"
      ,"\\; 2)2•◦ \\;\\widehat{JKL}=\\frac{\\pi}{3}"
      ,"\\; 3)1◦"
      ,"\\; 3)2•••◦ \\;\\mathcal{S}=\\{(" <> show f1.fraction <> "," <> show (-f2.fraction)
                                     <> "),(" <> show (-f2.fraction) <> ","  <> show f1.fraction <> ")\\}"
      ,"\\; 3)3•\\; \\mathcal{S}=\\{(" <> show f1.fraction <> "," <> show f2.fraction
                                     <> "),(" <> show (-f2.fraction) <> ","  <> show (-f1.fraction) <> ")\\}"
      ,"\\; 4)1•◦"
      ,"\\; 4)2◦"
      ,"\\; 4)3•◦"
      ,"\\; 4)4◦"
      ,"\\; 4)5◦"
      ,"\\; 4)6◦"
]
  render $ if fromString val < 0 then foldr (<>) "" rep else ""
    
spacex :: Int -> String 
spacex n = foldr (<>) "" $ replicate n "\\;"

main :: Effect Unit
main = void $ unsafePartial do
  setup <- DOM.setup
  
  seed <- DOM.createElement "input" setup.document
  _ <- DOM.addEventListener (cb setup.document) DOM.change seed
  _ <- DOM.appendChild seed setup.body
  setTitle "Devoir 3 : Cercle trigonométrique / Equations de degré 2"
  raw "Nom:"
  render $ spacex 40
  raw "Prénom:"
  render $ spacex 40
  raw "Classe:"
  pure unit
  
