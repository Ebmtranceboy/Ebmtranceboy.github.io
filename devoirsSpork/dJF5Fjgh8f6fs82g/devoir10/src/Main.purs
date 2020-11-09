module Main where

import Prelude

import Control.Monad.State (State)
import Data.Array ((!!)) as Array
import Data.Array (replicate)
import Data.Const (Const)
import Data.Foldable (foldr)
import Data.Int (floor)
import Data.Maybe (maybe, fromJust)
import Data.Number (fromString)
import Data.Ord (abs)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Rand (Rand, rand, unsort)
import SporKaTeX (RenderEffect(..), runRenderEffect, fromIncremental, put, get) as KaTeX
import SporKaTeX (t, nl, b, setTitle, mathEquation, mathInline)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (liftNat, merge, never)
import Data.Rational (Rational, fromInt, numerator, denominator)

type Model =
  { disabled :: Boolean
  , code :: Int
  }

initialModel ∷ Model
initialModel =
  { disabled: true
  , code: 0
  }

data Action
  = None
  | RenderElement String H.ElementRef
  | RenderContent String

update ∷ Model → Action
       → App.Transition KaTeX.RenderEffect Model Action
update model = case _ of
  None →
    App.purely model

  RenderContent cmd ->
    App.purely $ maybe model (\ nb->
      let code = floor nb
      in model { disabled = false
               , code = code
               })
               $ fromString cmd

  RenderElement str ref ->
    let
      effects = case ref of
        H.Created el → App.lift (KaTeX.RenderEffect str el None)
        H.Removed _  → mempty
    in
      { model, effects }

render ∷ Model → H.Html Action
render model =
  H.div
    []
    [ header
    , content model
    , command
    ]

header :: H.Html Action
header = H.div [] $ KaTeX.fromIncremental $ do
  let m t = mathInline RenderElement t
  setTitle "Devoir 10 : Suites arithmétiques / Suites géométriques / Fonctions dérivées"
  nl
  KaTeX.put $ H.div [ H.attr "style" "display: grid; grid-template-columns: 1fr 1fr 1fr;"]
      [ H.label [] [H.text "Nom:"]
      , H.label [] [H.text "Prénom:"]
      , H.label [] [H.text "Classe:"]
      ]
  KaTeX.put $ H.ul []
      [ H.li [] [H.text "10 questions : pour chacune d'elle, indiquer la (ou les) bonne(s) réponse(s)"]
      , H.li [] (KaTeX.fromIncremental $ do
           t "1 point par question"
           KaTeX.get)
      , H.li [] [H.text "sans document"]
      , H.li [] [H.text "calculatrice autorisée"]
      ]
  KaTeX.get

content :: Model -> H.Html Action
content model =
  if model.disabled
    then H.empty
    else H.div [] $ KaTeX.fromIncremental $ lines model

command :: H.Html Action
command = H.input [ H.autofocus true
                  , H.onValueChange (H.always RenderContent)]

texLeaf :: String -> H.Html Action
texLeaf str = H.label [H.ref (H.always (RenderElement str))] []

nth :: forall a. Array a -> Int -> a
nth arr n = unsafePartial fromJust $ arr Array.!! n

infixr 6 nth as !!

intMax = 7 :: Int

randPositive :: Rand -> Rand
randPositive r =
  if r.val `mod` intMax == 0
    then randPositive (rand r)
    else r

-- m > 0
randAffine :: Rand -> {m :: Int, p :: Int, r :: Rand}
randAffine r =
  let rm = randPositive r
      rp = rand rm
      rsp = rand rp
      sp = if rsp.val `mod` 2 == 0 then 1 else -1
  in { m: rm.val `mod` intMax
     , p: sp * (rp.val `mod` intMax)
     , r: rsp
     }

showHead :: Int -> String -> String
showHead n suffix =
    (case unit of
      unit | n == 1    -> ""
           | n == -1   -> "-"
           | otherwise -> show n)
    <> suffix

showInline :: Int -> String -> String
showInline n suffix =
  case unit of
    unit | n == 1 && suffix == ""  -> "+1"
         | n == -1 && suffix == "" -> "-1"
         | n == 1                  -> "+" <> suffix
         | n == -1                 -> "-" <> suffix
         | n < 0                   -> show n <> suffix
         | n == 0                  -> ""
         | otherwise               -> "+" <> show n <> suffix

showAffine :: {m :: Int, p :: Int} -> String
showAffine {m,p} =
  showHead m "x"
  <> showInline p ""

showProp :: forall b. (String -> State (Array (H.Html Action)) b)
                     -> Array String -> Array Int -> H.Html Action
showProp m props ord =
  H.div [ H.attr "style" "display: grid; grid-template-columns: 1fr 1fr 1fr;"]
    [ H.label []
      ( KaTeX.fromIncremental $ do
          t "a) "
          _ <- m $ props !! (ord !! 0)
          KaTeX.get
      )
    , H.label []
      ( KaTeX.fromIncremental $ do
          t "b) "
          _ <- m $ props !! (ord !! 1)
          KaTeX.get
      )
    , H.label []
      ( KaTeX.fromIncremental $ do
          t "c) "
          _ <- m $ props !! (ord !! 2)
          KaTeX.get
      )
    ]

rev :: Array Int -> Array Int
rev [a,b,c] = case a,b,c of
  0,1,2 -> [0,1,2]
  0,2,1 -> [0,2,1]
  1,0,2 -> [1,0,2]
  2,1,0 -> [2,1,0]
  1,2,0 -> [2,0,1]
  2,0,1 -> [1,2,0]
  _,_,_ -> [a,b,c]
rev xs = xs

showSol :: Array Int -> Array Int -> String
showSol reps ord =
    let revOrd = rev ord
        showRep rep = case revOrd !! rep of
          0 -> "a)"
          1 -> "b)"
          _ -> "c)"
      in foldr (<>) "" $ showRep <$> reps

showFraction :: Rational -> String
showFraction r =
    if denominator r == 1
      then show $ numerator r
      else "\\frac{" <> show (numerator r) <> "}{" <> show (denominator r) <> "}"

lines :: Model -> State (Array (H.Html Action))
            (Array (H.Html Action))
lines model = do
  let m = mathInline RenderElement
      equation = mathEquation RenderElement
      odd = 2 * abs model.code + 1
      r0 = rand { val: odd
                , gen: 0
                , seed: odd*odd
                }

  nl
  nl

  let rk1 = randPositive r0
      k1 = rk1.val `mod` intMax
  b "1. "
  t "Soit "
  m $ "(v_n)_{n\\in\\mathbb{N}}"
  t " une suite définie par son premier terme et la relation "
  m $ "v_{n+1}=\\left(" <> show k1 <> "+\\dfrac{v_n}{" <> show (2*k1) <> "}\\right)^2-\\dfrac{v_n^2}{" <> show (4*k1*k1) <> "}"
  t ". "
  nl
  m "(v_n)"
  t " est une suite :"
  let prop1 = ["\\mathrm{géométrique}\\;\\mathrm{de}\\;\\mathrm{raison}\\;" <> show k1
              , "\\mathrm{arithmétique}\\;\\mathrm{de}\\;\\mathrm{raison}\\;" <> show (k1*k1)
              , "\\mathrm{ni}\\;\\mathrm{arithmétique}\\;\\mathrm{ni}\\;\\mathrm{géométrique}\\;"]
      rep1 = [1]
      rs1 = rand $ rand $ rand rk1
      ord1 = unsort 3 rk1
  nl
  nl
  KaTeX.put $ showProp m prop1 ord1

  nl
  nl
  let rk2 = randPositive rs1
      k2 = 2 * (rk2.val `mod` intMax) + 1
  b "2. "
  t "Soit "
  m $ "(u_n)_{n\\in\\mathbb{N}}"
  t " une suite définie par son premier terme et la relation "
  m $ "u_{n+1}=\\left(\\dfrac{" <> show k2 <> "}{2}+u_n\\right)^2-\\left(\\dfrac{" <> show (k2*k2) <> "}{4}+u_n^2 \\right)"
  t ". "
  nl
  m "(u_n)"
  t " est une suite :"
  let prop2 = ["\\mathrm{géométrique}\\;\\mathrm{de}\\;\\mathrm{raison}\\;" <> show k2
              , "\\mathrm{arithmétique}\\;\\mathrm{de}\\;\\mathrm{raison}\\;\\dfrac{" <> show k2 <> "}{2}"
              , "\\mathrm{ni}\\;\\mathrm{arithmétique}\\;\\mathrm{ni}\\;\\mathrm{géométrique}\\;"]
      rep2 = [0]
      rs2 = rand $ rand $ rand rk2
      ord2 = unsort 3 rk2
  nl
  nl
  KaTeX.put $ showProp m prop2 ord2

  nl
  nl
  let ru3 = randPositive rs2
      u3 = ru3.val `mod` intMax
      rr3 = randPositive $ rand ru3
      r3 = 1 + (rr3.val `mod` intMax)
      rn3 = randPositive $ rand rr3
      n3 = 15 + (rn3.val `mod` intMax)

  b "3. "
  t "La somme "
  m $ show u3 <> "+" <> show (u3+r3) <> "+" <> show (u3+2*r3) <> "+\\cdots +" <> show (u3+n3*r3)
  t " est égale à :"
  nl
  let prop3 = [ show $ ((n3+1)*(u3 + u3 +n3*r3)) `div` 2
              , show u3 <> "\\times\\dfrac{1-" <> show r3  <> "^{" <> show (n3+1) <> "}}{1-" <> show r3 <> "}"
              , show (n3+1) <> "\\times\\dfrac{" <> show u3 <> "+" <> show (u3 +n3*r3) <> "}{2}"
              ]
      rep3 = [0,2]
      rs3 = rand $ rand $ rand rn3
      ord3 = unsort 3 rn3
  nl
  nl
  KaTeX.put $ showProp m prop3 ord3

  nl
  nl
  let qpos r =
        if r.val `mod` 5 == 0
          then qpos (rand r)
          else r
      rq4 = qpos $ rand rs3
      q4 = 1 + (rq4.val `mod` 5)
      rn4 = randPositive $ rand rq4
      n4 = 5 + (rn4.val `mod` 5)
      qs 1 = q4
      qs n = q4 * (qs $ n - 1)

  b "4. "
  t "La somme "
  m $ show 1 <> "+" <> show q4 <> "+" <> show (q4*q4) <> "+\\cdots +" <> show (qs n4)
  t " est égale à :"
  nl
  let prop4 = [ show $ (1 - (qs $ n4+1)) `div` (1 - q4)
              , "\\dfrac{1-" <> show q4  <> "^{" <> show (n4+1) <> "}}{1-" <> show q4 <> "}"
              , show (n4+1) <> "\\times\\dfrac{" <> show 1 <> "+" <> show (qs n4) <> "}{2}"
              ]
      rep4 = [0,1]
      rs4 = rand $ rand $ rand rn4
      ord4 = unsort 3 rn4
  nl
  nl
  KaTeX.put $ showProp m prop4 ord4

  nl
  nl

  b "5. "
  let rden5 = randPositive $ rand rs4
      den5 = 1 + (rden5.val `mod` intMax)
  m $ "\\lim\\limits_{n\\rightarrow + \\infty} \\left(\\dfrac{1}{" <> show den5 <> "}\\right)^n="
  let prop5 = [ "0"
              , "+\\infty"
              , "\\dfrac{1}{" <> show den5 <> "}"
              ]
      rep5 = [0]
      rs5 = rand $ rand $ rand rden5
      ord5 = unsort 3 rden5
  nl
  nl
  KaTeX.put $ showProp m prop5 ord5

  nl
  nl

  let {m: m6', p: p6, r: r6} = randAffine rs5
      m6 = 2*m6'+1
  b "6. "
  t "Soit "
  m "f"
  t " la fonction définie sur "
  m $ " ]" <> showFraction (fromInt (-p6) / fromInt m6) <> "; + \\infty["
  t " par "
  m $ " f(x)=\\sqrt{" <> showAffine {m: m6, p: p6} <> "}"
  t "."
  nl
  t "Alors "
  m "f'(x)="
  let prop6 = [ "\\dfrac{" <> show m6 <> "}{2\\sqrt{" <> showAffine {m: m6, p: p6} <> "}}"
              , "\\sqrt{" <> show m6 <> "}"
              , "\\dfrac{1}{" <> showAffine {m: m6, p: p6} <> "}"
              ]
      rep6 = [0]
      rs6 = rand $ rand $ rand r6
      ord6 = unsort 3 r6
  nl
  nl
  KaTeX.put $ showProp m prop6 ord6

  nl
  nl

  let {m: m7', p: p7, r: r7} = randAffine rs6
      m7 = 1 + m7'
  b "7. "
  t "Soit "
  m "f"
  t " la fonction définie sur "
  m $ " ]" <> showFraction (fromInt (-p7) / fromInt m7) <> "; + \\infty["
  t " par "
  m $ " f(x)=\\dfrac{1}{" <> showAffine {m: m7, p: p7} <> "}"
  t "."
  nl
  t "Alors "
  m "f'(x)="
  let prop7 = [ "\\dfrac{" <> show (-m7) <> "}{\\left(" <> showAffine {m: m7, p: p7} <> "\\right)^2}"
              , "\\dfrac{1}{\\left(" <> showAffine {m: m7, p: p7} <> "\\right)^2}"
              , "\\dfrac{1}{" <> show m7 <> "}"
              ]
      rep7 = [0]
      rs7 = rand $ rand $ rand r7
      ord7 = unsort 3 r7
  nl
  nl
  KaTeX.put $ showProp m prop7 ord7

  nl
  nl

  let {m: m8', p: p8, r: r8} = randAffine rs7
      m8 = 1 + m8'
  b "8. "
  t "Soit "
  m "f"
  t " la fonction définie sur "
  m "\\mathbb{R}"
  t " par "
  m $ " f(x)=\\left(" <> showAffine {m: m8, p: p8} <> "\\right)^3"
  t "."
  nl
  t "Alors "
  m "f'(x)="
  let prop8 = [ show (3 * m8) <> "\\left(" <> showAffine {m: m8, p: p8} <> "\\right)^2"
              , "3\\left(" <> showAffine {m: m8, p: p8} <> "\\right)^2"
              , show (m8 * m8 * m8)
              ]
      rep8 = [0]
      rs8 = rand $ rand $ rand r8
      ord8 = unsort 3 r8
  nl
  nl
  KaTeX.put $ showProp m prop8 ord8

  nl
  nl

  let ra9 = randPositive $ rand rs8
      a9 = 2 * (ra9.val `mod` intMax)
      rb9 = randPositive $ rand ra9
      b9 = 2 * (rb9.val `mod` intMax) + 1
  b "9. "
  t "Soit "
  m "f"
  t " la fonction définie sur "
  m "\\mathbb{R}"
  t " par "
  m $ " f(x)=(x-" <> show a9 <> ")(x-" <> show b9 <> ")"
  t "."
  nl
  t "Alors "
  m "f'(x)="
  let prop9 = [ showAffine {m: 2, p: -(a9+b9)}
              , "1"
              , show (a9+b9)
              ]
      rep9 = [0]
      rs9 = rand $ rand $ rand rb9
      ord9 = unsort 3 rb9
  nl
  nl
  KaTeX.put $ showProp m prop9 ord9

  nl
  nl

  b "10. "
  t "Soit "
  m "f"
  t " la fonction définie sur "
  m "\\mathbb{R}"
  t " par "
  m $ " f(x)=(x-" <> show a9 <> ")(x-" <> show b9 <> ")"
  t "."
  let prop10 =[ "f'(" <> showFraction (fromInt (a9+b9) / fromInt 2) <> ")=0"
              , "f'(" <> show (a9+b9) <> ")=" <> show (a9+b9)
              , "f'(" <> showFraction (fromInt (a9+b9) / fromInt 2) <> ")=" <> showFraction (fromInt (a9+b9) / fromInt 2)
              ]
      rep10 = [0,1]
      rs10 = rand $ rand $ rand rs9
      ord10 = unsort 3 rs9
  nl
  nl
  KaTeX.put $ showProp m prop10 ord10

  nl
  nl


  if model.code < 0
    then do
          nl
          t "réponses: "
          t " 1. "
          t $ showSol rep1 ord1
          t " 2. "
          t $ showSol rep2 ord2
          t " 3. "
          t $ showSol rep3 ord3
          t " 4. "
          t $ showSol rep4 ord4
          t " 5. "
          t $ showSol rep5 ord5
          t " 6. "
          t $ showSol rep6 ord6
          t " 7. "
          t $ showSol rep7 ord7
          t " 8. "
          t $ showSol rep8 ord8
          t " 9. "
          t $ showSol rep9 ord9
          t " 10. "
          t $ showSol rep10 ord10
    else pure mempty

  KaTeX.get


vector :: Int -> Int -> String
vector x y =
  "\\begin{pmatrix}" <> show x <> "\\\\" <> show y <> "\\end{pmatrix}"

app ∷ App.App KaTeX.RenderEffect (Const Void) Model Action
app =
  { render
  , update
  , subs: const mempty
  , init: App.purely initialModel
  }

spacex :: Int -> String
spacex n = foldr (<>) "" $ replicate n "\\;"

main ∷ Effect Unit
main = do
  inst ←
    App.makeWithSelector
      (liftNat KaTeX.runRenderEffect `merge` never)
      app
      "#app"
  inst.run
