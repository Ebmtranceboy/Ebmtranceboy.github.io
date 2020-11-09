module Main where

import Prelude
import Effect (Effect)
import Data.Maybe(Maybe(..),fromJust)
import Partial.Unsafe(unsafePartial)
import KaTeX (cat, equation, list, newline, raw, render, setTitle, subraw)
import DOM.Editor as DOM
import Data.Array(replicate, (!!), (\\), scanl, last)
import Data.Array(length) as Array
import Data.Foldable(foldr)
import Data.Ord(abs) as Ord

foreign import fromString :: String -> Int 

primes :: Array Int
primes = [2,2,2,2,2,2,3,3,3,3,3,5,5,5,5]

avgNbFactors = 2 :: Int

newtype Fraction = Fraction {num :: Int, den :: Int}

instance eqFraction :: Eq Fraction where
  eq (Fraction f1) (Fraction f2) = 
    (f1.num == 0 && f2.num == 0) || (f1.num == f2.num && f1.den == f2.den) 

instance showFraction :: Show Fraction where
  show (Fraction f) = 
    if f.den == 1 
      then show f.num
      else "\\frac{" <> show f.num <> "}{" <> show f.den <> "}"

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
   in m2 <> m1 <> inline c 

inline :: Fraction -> String
inline c =
  case unit of
       unit | c < zero -> "-" <> (show $ abs c)
            | c == zero -> ""
            | otherwise -> "+" <> show c

instance semiRingFraction :: Semiring Fraction where
  add (Fraction f1) (Fraction f2) = 
    let d =  f1.den * f2.den
        n = f1.num * f2.den + f2.num * f1.den
        g = gcd (Ord.abs n) d
    in Fraction {num: n `div` g, den: d `div` g}
  zero = Fraction {num: 0, den: 1}
  mul (Fraction f1) (Fraction f2) = 
    let n = f1.num * f2.num
        d = f1.den * f2.den
        g = gcd (Ord.abs n) d
     in Fraction {num: n `div` g, den:  d `div` g}
  one = Fraction {num: 1, den: 1}

instance ringFraction :: Ring Fraction where
  sub f1 f2 = f1 + lift (-1) * f2

instance ordFraction :: Ord Fraction where
  compare f1 f2 = 
    let Fraction f = f1 - f2 
     in case unit of
             unit | f.num < 0 -> LT
                  | f.num > 0 -> GT
                  | otherwise -> EQ

abs :: Fraction -> Fraction
abs (Fraction {num, den}) = Fraction {num: Ord.abs num, den} 

instance divisionRingFraction :: DivisionRing Fraction where
  recip (Fraction f) = 
    if f.num < 0 
      then Fraction {num: -f.den, den: -f.num}
      else Fraction {num: f.den, den: f.num}

instance commutativeRingFraction :: CommutativeRing Fraction

instance euclideanRingFraction :: EuclideanRing Fraction where
  degree = const 1
  div f1 f2 = f1 * recip f2
  mod _ _ = zero

lift :: Int -> Fraction
lift n = Fraction {num: n, den: 1}

alpha :: Fraction -> Fraction -> Fraction -> Fraction
alpha a b c = -b/a/lift 2

beta :: Fraction -> Fraction -> Fraction -> Fraction
beta a b c = 
  let al = alpha a b c
      f x = a * x * x + b * x + c
   in f al

delta :: Fraction -> Fraction -> Fraction -> Fraction
delta a b c = b * b - lift 4 * a * c

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

aVeryBigPowerOf2 = 33554432 :: Int -- 2^25

type Rand = {val :: Int, gen :: Int, seed :: Int}

middle :: Int -> Int
middle nn = 
  let n0 = nn `mod` 100
      n3' = nn `mod` 1000000
      n3 = nn - ((nn-n3') `div` 1000000) * 1000000 
   in (n3-n0) `div` 100 

rand :: Rand -> Rand
rand {val, gen, seed} = 
  { val: middle $ (val * val + gen) `mod` 100000000
  , gen: (gen + seed) `mod` 100000000
  , seed}

{-
rand :: Int -> Rand -> Rand
rand max {val, gen} = 
  let y = (5*gen + 1) `mod` aVeryBigPowerOf2
  in {val: y `mod` max, gen: y} 
  -}

{-
rand :: Int -> Rand -> Rand
rand max {val, gen} = 
  let y = (gen * (4*gen + 1) +1) `mod` aVeryBigPowerOf2
  in {val: y `mod` max, gen: y} 
  -}

cb :: DOM.Document -> DOM.Event -> Effect Unit
cb doc = unsafePartial $ \ev -> do
  val <- DOM.inputedValueFromEvent ev
  let odd = 2 * (Ord.abs $ fromString val) + 1
  let r0 = {val: odd, gen: 0, seed: odd * odd}
  newline
  
  list [ cat [subraw "5 questions"] 
       , cat [subraw "1 point par bonne réponse"]
       , cat [ subraw "toute valeur numérique "
             , subraw "à donner sous forme entière "
             , subraw "ou rationnelle irréductible"]
      , cat [subraw "calculatrice autorisée"]]
  
  newline
  raw "1) Deux nombres "
  render "x_1"
  raw " et "
  render "x_2"
  raw " vérifient "
  let f1 = randFraction r0
  let f2 = randFraction f1.nextRand 
  render $ "\\left\\{\\begin{array}{l}x_1+x_2=" 
                    <> (show $ f1.fraction + f2.fraction) 
                    <> "\\\\ x_1x_2 = " 
                    <> (show $ f1.fraction * f2.fraction)
                    <> "\\end{array}\\right."
  newline
  raw "Donner l'ensemble des couples "
  render "(x_1, x_2)"
  raw " possibles:"
  newline 
  newline
  newline

  raw "2) Soit "
  render "f"
  raw " la fonction polynôme du second degré définie par "
  let f3 = randFraction f2.nextRand
  let f4 = randFraction f3.nextRand
  let f5 = randFraction f4.nextRand
  let a2 = f3.fraction
  let b2 = f4.fraction
  let c2 = f5.fraction
  render $ "f(x)=" <> showTrinom a2 b2 c2
  raw " pour tout "
  render "x\\in\\mathbb{R}"
  raw ". "
  newline
  raw "Donner la forme canonique de "
  render "f"
  raw " :"
  newline
  newline
  newline

  newline
  raw "3) Calculer le discriminant de "
  let f6 = randFraction f5.nextRand
  let f7 = randFraction f6.nextRand
  let f8 = randFraction f7.nextRand
  let a3@(Fraction acore3) = f6.fraction
  let b3@(Fraction bcore3) = f7.fraction
  let c3@(Fraction ccore3) = f8.fraction  
  let l3 = lcm (acore3.den) (lcm bcore3.den ccore3.den)
  let a3'@(Fraction acore3') = lift l3 * a3
  let b3'@(Fraction bcore3') = lift l3 * b3
  let c3'@(Fraction ccore3') = lift l3 * c3
  let g3 = gcd (Ord.abs $ acore3'.num) (gcd (Ord.abs bcore3'.num) (Ord.abs ccore3'.num))
  render $ showTrinom (a3' / lift g3) (b3' / lift g3) (c3' / lift g3)
  raw "."
  newline
  raw "En déduire le nombre de solutions réelles de l'équation "
  equation $ (if a3 == a3' / lift g3
             then showTrinom (a3 / lift (-2))  (b3 / lift (-2))  (c3 / lift (-2))
             else showTrinom a3 b3 c3) <> "=0"
  raw "On donnera la valeur du discriminant "
  raw "ainsi que le nombre de solutions."
  newline
  raw "Même s'il y a des solutions, on ne demande pas de calculer leur valeur:"
  newline
  newline
  newline

  newline
  raw "4) Résoudre dans "
  render "\\mathbb{R}:"
  let f9 = randFraction f8.nextRand
  let f10 = randFraction f9.nextRand
  let f11 = randFraction f10.nextRand
  let a4@(Fraction acore4) = f9.fraction
  let x14 = f10.fraction
  let x24 = f11.fraction
  let b4@(Fraction bcore4) = -a4*(x14+x24)
  let c4@(Fraction ccore4) = a4*x14*x24
  let l4 = lcm (acore4.den) (lcm bcore4.den ccore4.den)
  let a4'@(Fraction acore4') = lift l4 * a4
  let b4'@(Fraction bcore4') = lift l4 * b4
  let c4'@(Fraction ccore4') = lift l4 * c4
  let g4 = gcd (Ord.abs $ acore4'.num) 
               (gcd (Ord.abs bcore4'.num) (Ord.abs ccore4'.num))
  equation $ showTrinom (a4' / lift g4) 
                        (b4' / lift g4) 
                        (c4' / lift g4) <> "=0"
  newline
  newline
  newline

  newline
  raw "5) Soit "
  render "f"
  raw " la fonction définie sur "
  render "\\mathbb{R}"
  raw " par "
  let f12 = randFraction f11.nextRand
  let f13 = randFraction f12.nextRand
  let a5 = f12.fraction
  let b5 = f13.fraction
  render $ "f(x)=" <> showTrinom a5 b5 zero <> "+c"
  raw " où "
  render "c"
  raw " est un nombre réel."
  newline
  raw "Déterminer la valeur de "
  render "c"
  raw " pour laquelle "
  render "f"
  raw " admet une unique racine (on dit aussi racine \"double\") "
  render "x_0"
  raw ", puis calculer "
  render "x_0"
  raw ":"
  newline
  newline
  newline

  
  newline
  let alpha2 = alpha a2 b2 c2
      beta2 = beta a2 b2 c2
      delta3 = delta (a3' / lift g3) (b3' / lift g3) (c3' / lift g3)
      nbsol3 = case unit of
                    unit | delta3 < zero -> "pas\\;de\\; solution"
                         | delta3 == zero -> "unique\\; solution"
                         | otherwise -> "deux\\; solutions\\; distinctes"
      sol4 = 
        if x14 == x24 
          then "\\{" <> show x14 <> "\\}"
          else "\\{" <> show x14 <> "," <> show x24 <> "\\}"

      rep = ["réponses: 1)"
      ,"\\mathcal{S}=" 
              <> (if f1.fraction == f2.fraction
                 then "\\{(" <> show f1.fraction <> ","  
                          <> show f2.fraction <> ")\\}"
                else "\\{(" <> show f1.fraction <> ","
                          <> show f2.fraction <> "),("
                        <> show f2.fraction <> ","
                        <> show f1.fraction <> ")\\}"),
                        "\\; 2) ","f(x)="<>show a2<>"(x"<> inline (-alpha2)
                                              <>")^2"<> inline beta2 
                        ,"\\; 3)\\Delta =" <> show delta3<>",\\; "<>nbsol3 
                        , "\\; 4)\\mathcal{S}=" <> sol4, "\\; 5)","c="
                        , show $ b5*b5/a5/lift 4,"\\; x_0="
                        , show $ -b5/a5/lift 2]
  render $ if fromString val < 0 then foldr (<>) "" rep else ""
    
spacex :: Int -> String 
spacex n = foldr (<>) "" $ replicate n "\\;"

main :: Effect Unit
main = void $ unsafePartial do
  setup <- DOM.setup
  
  seed <- DOM.createElement "input" setup.document
  _ <- DOM.addEventListener (cb setup.document) DOM.change seed
  _ <- DOM.appendChild seed setup.body
  setTitle "Devoir 2 : Second degré"
  raw "Nom:"
  render $ spacex 40
  raw "Prénom:"
  render $ spacex 40
  raw "Classe:"
  pure unit
  
