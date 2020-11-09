module Main where

import Prelude
import Effect (Effect)
import Data.Maybe(Maybe(..),fromJust)
import Partial.Unsafe(unsafePartial)
import KaTeX (cat, list, newline, raw, render
             , setTitle, subraw, subsection)
import SVG.Geometry (segment,point)
import SVG.Render as SVG
import DOM.Editor as DOM
import Data.Array(fromFoldable,replicate, (!!), (\\), scanl, last,uncons)
import Data.Array(length) as Array
import Data.Foldable(foldr,all)
import Data.Ord(abs) as Ord
import Fraction (Fraction(..), fromInt)
import Rand (Rand, rand)
import Data.Map (Map, empty, insert, lookup, values)

foreign import fromString :: String -> Int 

type Key = String
type Experience = Map Key Fraction

cond :: Key -> Key -> Key -> Experience -> Maybe Experience
cond pc pi pr e = 
  case [lookup pc e, lookup pi e, lookup pr e] of
       [Just c, Just i, Nothing] -> Just $ insert pr (i / c) e
       [Just c, Nothing, Just r] -> Just $ insert pi (c * r) e
       [Nothing, Just i, Just r] -> Just $ insert pc (i / r) e
       otherwise -> Nothing

contr :: Key -> Key -> Experience -> Maybe Experience
contr pa pna e = 
  case [lookup pa e, lookup pna e] of
       [Just a, Nothing] -> Just $ insert pna (fromInt 1 - a) e
       [Nothing, Just na] -> Just $ insert pa (fromInt 1 - na) e
       otherwise -> Nothing

ptot :: Key -> Key -> Key -> Experience -> Maybe Experience
ptot pa pab panb e = 
  case [lookup pa e, lookup pab e, lookup panb e] of
       [Just a, Just ab, Nothing] -> Just $ insert panb (a - ab) e
       [Just a, Nothing, Just anb] -> Just $ insert pab (a - anb) e
       [Nothing, Just ab, Just anb] -> Just $ insert pa (ab + anb) e
       otherwise -> Nothing

dep :: Key -> Key -> Key -> Key -> Experience -> Maybe Experience
dep pa pb pgab pgnab e = 
  case [lookup pa e, lookup pb e, lookup pgab e, lookup pgnab e] of
       [Just a, Just b, Just gab, Nothing] -> 
          Just $ insert pgnab ((b-gab*a)/(fromInt 1 -a)) e
       [Just a, Just b, Nothing, Just gnab] -> 
          Just $ insert pgab ((b-gnab*(fromInt 1 - a))/a) e
       [Just a, Nothing, Just gab, Just gnab] -> 
          Just $ insert pb (gab*a+gnab*(fromInt 1 - a)) e
       [Nothing, Just b, Just gab, Just gnab] -> 
          Just $ insert pa ((b-gnab)/(gab-gnab)) e
       otherwise -> Nothing

complete :: Experience -> Experience
complete e =
  let rules = [ contr "pA" "pnA"
              , contr "pB" "pnB"
              , contr "pgAB" "pgAnB"
              , contr "pgnAB" "pgnAnB"
              , contr "pgBA" "pgBnA"
              , contr "pgnBA" "pgnBnA"
              , cond "pgAB" "pAB" "pA"
              , cond "pgAnB" "pAnB" "pA"
              , cond "pgnAB" "pnAB" "pnA"
              , cond "pgnAnB" "pnAnB" "pnA"
              , cond "pgBA" "pAB" "pB"
              , cond "pgBnA" "pnAB" "pB"
              , cond "pgnBA" "pAnB" "pnB"
              , cond "pgnBnA" "pnAnB" "pnB"
              , ptot "pA" "pAB" "pAnB"
              , ptot "pnA" "pnAB" "pnAnB"
              , ptot "pB" "pAB" "pnAB"
              , ptot "pnB" "pAnB" "pnAnB"
              , dep "pA" "pB" "pgAB" "pgnAB"]
      f xs = 
        case uncons xs of
             Nothing -> e
             Just {head: rule, tail} ->
               case rule e of
                  Just e' -> complete e'
                  Nothing -> f tail
  in f rules

primes :: Array Int
primes = [2,2,2,2,2,2,3,3,3,3,3,5,5,5,5]

avgNbFactors = 2 :: Int

randProba :: Rand -> {probability :: Fraction, nextRand :: Rand}
randProba = unsafePartial \ r -> 
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
      prime = unsafePartial $ \ ix -> fromJust $ primes !! ix 
      nums = prime <$> (\rnd -> rnd.val `mod` (Array.length primes)) 
                   <$> nrands
      dens = prime <$> (\rnd -> rnd.val `mod` (Array.length primes)) 
                   <$> drands
      num = foldr (*) 1 $ nums \\ dens
      den = foldr (*) 1 $ dens \\ nums
   in case unit of
           unit | num < den -> {probability: Fraction {num, den}, nextRand}
                | num > den -> { probability: Fraction {num: den, den: num}
                               , nextRand}
                | otherwise -> randProba nextRand 

validExperience :: Experience -> Boolean
validExperience e =
      let valid x = fromInt 0 < x && x < fromInt 1
          vs = fromFoldable $ values e
      in Array.length vs == 16 && all valid vs

print :: Experience -> Key -> String
print e key = 
        case lookup key e of
             Just p -> show p
             Nothing -> ""

infixr 6 print as °

tree :: DOM.Document -> Experience  -> Effect Unit
tree doc e = do
-- https://www.sitepoint.com/how-to-translate-from-dom-to-svg-coordinates-and-back-again/
  thisPosition <- DOM.getElementById "description" doc
  svg <- DOM.newSVG "position: absolute; width: 500px; height: 400px;" thisPosition
  let root = point "" 12.0 175.0
  let nodeA = point "" 100.0 92.0
  let nodeA' = point "" 100.0 244.0
  let startA = point "" 126.0 80.0
  let endAB = point "" 202.0 24.0
  let endAB' = point "" 202.0 136.0
  let startA' = point "" 126.0 264.0
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

  SVG.render' context $  segment frameTL frameTR Nothing
  SVG.render' context $  segment frameTR frameBR Nothing
  SVG.render' context $  segment frameBR frameBL Nothing
  SVG.render' context $  segment frameBL frameTL Nothing

  SVG.render' context $ segment root nodeA Nothing
  SVG.render' context $ segment root nodeA' Nothing 
  SVG.render' context $ segment startA endAB  Nothing
  SVG.render' context $ segment startA endAB'  Nothing
  SVG.render' context $ segment startA' endA'B  Nothing
  SVG.render' context $ segment startA' endA'B'  Nothing
  
  render $ "\\begin{array}{ccccccccc} & & & & & & & & B \\\\ "
          <> "& & & & & " <> e°"pgAB" <> " \\\\ \\\\ "
          <> "& & & & A \\\\ "
          <> "&" <> e°"pA" <> " \\\\ "
          <> "& & & & &" <> e°"pgAnB" <> " \\\\ "
          <> "& & & & & & & &" <> "\\overline{B} \\\\ "
          <> "\\cdot \\\\ "
          <> "& & & & & & & &" <> "B \\\\ "
          <> "& & & & & " <> e°"pgnAB" <> " \\\\ "
          <> "&" <> e°"pnA" <> " \\\\ "
          <> "& & & & \\overline{A} \\\\ \\\\"
          <> "& & & & & " <> e°"pgnAnB" <> " \\\\ "
          <> "& & & & & & & & \\overline{B} \\end{array}"
  let dress str x k = 
        case lookup k x of
           Just v -> "&&&" <> str <> show v <> " \\\\"
           Nothing -> "" 
  let pB' = dress "P(B)=" e "pB"
  let pnB' = dress "P(\\overline{B})=" e "pnB"
  let pAB' = dress "P(A\\cap B)=" e "pAB"
  let pAnB' = dress "P(A\\cap\\overline{B})=" e "pAnB"
  let pnAB' = dress "P(\\overline{A}\\cap B)=" e "pnAB"
  let pnAnB' = dress "P(\\overline{A}\\cap\\overline{B})="  e "pnAnB"
  let pgBA' = dress "P_B(A)=" e "pgBA"
  let pgBnA' = dress "P_B(\\overline{A})=" e "pgBnA"
  let pgnBA' = dress "P_{\\overline{B}}(A)=" e "pgnBA"
  let pgnBnA' = dress "P_{\\overline{B}}(\\overline{A})=" e "pgnBnA"
  render $ "\\begin{array}{llllllllllll}" <> pB' <> pnB' 
                                   <> pAB' <> pAnB' <> pnAB' <> pnAnB' 
                                   <> pgBA' <> pgBnA' <> pgnBA' <> pgnBnA' 
                                   <> "\\\\ & & & & & & & & & & & & \\\\" 
                                   <>"\\end{array}"

exercice :: Key -> Key -> Key -> Experience -> Experience 
exercice p1 p2 p3 ref = 
   insert p1 (unsafePartial $ fromJust $ lookup p1 ref) 
    $ insert p2 (unsafePartial $ fromJust $ lookup p2 ref) 
      $ insert p3 (unsafePartial $ fromJust $ lookup p3 ref) empty

majIndex = 12 :: Int 

setWithIndex :: Int -> Fraction -> Experience -> Experience
setWithIndex n f e = 
  insert (case n of
       0 -> "pA"
       1 -> "pnA"
       2 -> "pB"
       3 -> "pnB"
       4 -> "pAB"
       5 -> "pAnB"
       6 -> "pnAB"
       7 -> "pnAnB"
       8 -> "pgAB"
       9 -> "pgAnB"
       10 -> "pgnAB"
       11 -> "pgnAnB"
       12 -> "pgBA"
       13 -> "pgBnA"
       14 -> "pgnBA"
       otherwise -> "pgnBnA") f e

randExercise :: Rand -> {experience :: Experience, nextRand :: Rand}
randExercise r1 =
  let modMaj x = x `mod` majIndex
      f2 r = 
        let s = rand r
        in if modMaj s.val  == modMaj r.val
            then f2 s
            else s
      f3 r s = 
        let t = rand s
        in if modMaj t.val == modMaj r.val || modMaj t.val == modMaj s.val
             then f3 s t
             else t
      r2 = f2 r1
      r3 = f3 r1 r2
      {probability: p1, nextRand: r4} = randProba r3
      {probability: p2, nextRand: r5} = randProba r4
      {probability: p3, nextRand} = randProba r5
      e = setWithIndex (modMaj r1.val) p1
             $ setWithIndex (modMaj r2.val) p2
             $ setWithIndex (modMaj r3.val) p3 empty
   in if validExperience (complete e)
        then {experience: e, nextRand}
        else randExercise nextRand

 {-
            pA pgAB pgnAB
            pAB pnAB pgAnB
            pAB pAnB pnAB
            pA pnAB pgAB
            pnA pAB pnAnB
            pnAnB pgAB pgnAB
            pA pnB pgAB
            pnA pB pnAnB
            pB pgAB pgnAB
            pnB pnAB pgAnB
            pB pAnB pnAB
  -}

cb :: DOM.Document -> DOM.Event -> Effect Unit
cb doc = unsafePartial $ \ev -> do
  val <- DOM.inputedValueFromEvent ev
  let odd = 2 * (Ord.abs $ fromString val) + 1
  let r0 = {val: odd, gen: 0, seed: odd * odd}
  newline
  
  list [ cat [subraw "5 arbres pondérés à compléter à partir des hypothèses"] 
       , cat [subraw "1 point par arbre complet"]
       , cat [subraw "calculatrice autorisée"]
       , cat [subraw "documents autorisés"]
       , cat [subraw "toute valeur numérique sous forme fractionnaire"]]
  
  newline
 
  let r1 = rand r0
  let prob e = 
        if fromString val < 0
          then tree doc $ complete e
          else tree doc e
 -- subsection "1)"
  let {experience: e1, nextRand: r2} = randExercise r1
  prob e1

--  subsection "2)"
  let {experience: e2, nextRand: r3} = randExercise r2
  prob e2
  
  subsection ""  --3)
  let {experience: e3, nextRand: r4} = randExercise r3
  prob e3
  
  --subsection "4)"
  let {experience: e4, nextRand: r5} = randExercise r4
  prob e4
  
  subsection "" -- 5)
  let {experience: e5, nextRand: r6} = randExercise r5
  prob e5
    
spacex :: Int -> String 
spacex n = foldr (<>) "" $ replicate n "\\;"

main :: Effect Unit
main = void $ unsafePartial do
  setup <- DOM.setup
  
  seed <- DOM.createElement "input" setup.document
  _ <- DOM.addEventListener (cb setup.document) DOM.change seed
  _ <- DOM.appendChild seed setup.body
  
  setTitle "Devoir 4 : Probabilités conditionnelles"
  raw "Nom:"
  render $ spacex 40
  raw "Prénom:"
  render $ spacex 40
  raw "Classe:"

  _ <- DOM.focus seed
  pure unit
  
