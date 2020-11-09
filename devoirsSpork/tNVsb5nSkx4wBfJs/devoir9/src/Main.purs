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
import SporKaTeX (t, nl, b, setTitle, mathEquation, mathInline, em)
import Spork.App as App
import Spork.Html as H
import Spork.Interpreter (liftNat, merge, never)

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
  setTitle "Devoir 9 : Nombres dérivés / Droites et cercles repérés"
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

-- a non null
randTriplet :: Rand -> {a :: Int, b :: Int, c :: Int, r :: Rand}
randTriplet r =
  let ra = randPositive r
      rsa = rand ra
      sa = if rsa.val `mod` 2 == 0 then 1 else -1
      rb = rand rsa
      rsb = rand rb
      sb = if rsb.val `mod` 2 == 0 then 1 else -1
      rc = rand rsb
      rsc = rand rc
      sc = if rsc.val `mod` 2 == 0 then 1 else -1
  in { a: sa * (ra.val `mod` intMax)
     , b: sb * (rb.val `mod` intMax)
     , c: sc * (rc.val `mod` intMax)
     , r: rsc
     }

-- (a,b) not (0,0)
randTriple :: Rand -> {a :: Int, b :: Int, c :: Int, r :: Rand}
randTriple r =
  let ra = rand r
      rsa = rand ra
      sa = if rsa.val `mod` 2 == 0 then 1 else -1
      rb = rand rsa
      rsb = rand rb
      sb = if rsb.val `mod` 2 == 0 then 1 else -1
      rc = rand rsb
      rsc = rand rc
      sc = if rsc.val `mod` 2 == 0 then 1 else -1
      g = gcd (gcd (ra.val `mod` intMax) (rb.val `mod` intMax)) (rc.val `mod` intMax)
      a = sa * (ra.val `mod` intMax) ` div` g
      b = sb * (rb.val `mod` intMax) ` div` g
      c = sc * (rc.val `mod` intMax) ` div` g
    in if a == 0 && b == 0
      then randTriple rsc
      else {a, b, c, r: rsc}

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

showTrinom :: {a :: Int, b :: Int, c :: Int} -> String
showTrinom {a,b,c} =
  showHead a "x^2"
  <> showInline b "x"
  <> showInline c ""

showCartesian :: {a :: Int, b :: Int, c :: Int} -> String
showCartesian {a,b,c} =
  (case unit of
    unit | a == 0 -> showHead b "y"
         | b == 0 -> showHead a "x"
         | otherwise -> showHead a "x" <> showInline b "y")
          <> showInline c "" <> "=0"

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
  em "Dans tout le devoir, les coordonnées des points et des vecteurs sont données dans un repère orthonormé."
  nl
  nl
  t "Pour les questions "
  b "1."
  t " à "
  b "5."
  t ", on considère la fonction "
  m "f"
  t " définie sur "
  m "\\mathbb{R}"
  t " par "
  let {a: a1,b: b1, c: c1, r: r1} = randTriplet r0
  m $ "f(x)=" <> showTrinom {a: a1,b: b1, c: c1}
  t ". On note "
  m "\\mathcal{C}_f"
  t " sa courbe représentative. Enfin, "
  m "h"
  t " désigne un réel non nul."

  nl
  nl

  let rx1 = randPositive r1
      rsx1 = rand rx1
      sx1 = if rsx1.val `mod` 2 == 0 then 1 else -1
      x1 = sx1 * (rx1.val `mod` intMax)
  b "1. "
  t "Le taux de variation "
  m $ "\\tau_{" <> show x1 <> "}(h)"
  t " de "
  m "f"
  t " entre "
  m $ show x1
  t " et "
  m $ show x1 <> "+h"
  t " est égal à:"
  let prop1 = ["\\dfrac{f(" <> show x1 <> "+h)+f(" <> show x1 <> ")}{h}"
              , "f'(" <> show x1 <> ")"
              , "\\dfrac{f(" <> show x1 <> "+h)-f(" <> show x1 <> ")}{h}"]
      rep1 = [2]
      rs1 = rand $ rand $ rand rsx1
      ord1 = unsort 3 rsx1
  nl
  nl
  KaTeX.put $ showProp m prop1 ord1

  nl
  nl

  b "2. "
  t "Pour tout réel "
  m "h"
  t " non nul, "
  m $ "\\tau_{" <> show x1 <> "}(h)"
  t " est égal à:"

  let prop2 = [ showHead a1 "h" <> showInline (2 * a1 * x1 + b1) ""
              , showHead a1 "h" <> showInline (-a1 * x1 + b1) ""
              , showHead a1 "h" <> showInline (3 * a1 * x1 + b1) ""
              ]
      rep2 = [0]
      rs2 = rand $ rand $ rand rs1
      ord2 = unsort 3 rs1
  nl
  nl
  KaTeX.put $ showProp m prop2 ord2

  nl
  nl

  b "3. "
  m "f"
  t " est dérivable en "
  m $ show x1
  t " et "
  m $ "f'(" <> show x1 <> ")"
  t " est égal à:"
  let prop3 = [ show $ 2 * a1 * x1 + b1
              , show $ -a1 * x1 + b1
              , show $ 3 * a1 * x1 + b1]
      rep3 = [0]
      rs3 = rand $ rand $ rand rs2
      ord3 = unsort 3 rs2
  nl
  nl
  KaTeX.put $ showProp m prop3 ord3

  nl
  nl

  b "4. "
  let x4 = -x1
      d4 = 2 * a1 * x4 + b1
  t "On admet que "
  m $ "f'(" <> show x4 <> ")=" <> show d4
  t ". La tangente à "
  m "\\mathcal{C}_f"
  t " au point d'abscisse "
  m $ show x4
  t ":"

  let prop4 = [ "\\mathrm{passe}\\;\\mathrm{par}\\;\\mathrm{le}\\;\\mathrm{point}\\;\\mathrm{de}\\;\\mathrm{coordonnées}\\;(" <> show x4 <> "," <> show (-a1 * x4 * x4 + b1 * x4 + c1) <> ")"
              , "\\mathrm{passe}\\;\\mathrm{par}\\;\\mathrm{le}\\;\\mathrm{point}\\;\\mathrm{de}\\;\\mathrm{coordonnées}\\;(" <> show x4 <> "," <> show (a1 * x4 * x4 + b1 * x4 + c1) <> ")"
              , "\\mathrm{admet}\\;\\mathrm{pour}\\;\\mathrm{coefficient}\\;\\mathrm{directeur}\\;" <> show d4
              ]
      rep4 = [1,2]
      rs4 = rand $ rand $ rand rs3
      ord4 = unsort 3 rs3
  nl
  nl
  KaTeX.put $ showProp m prop4 ord4

  nl
  nl

  b "5. "
  t "L'équation réduite de la tangente à "
  m "\\mathcal{C}_f"
  t " au point d'abscisse "
  m $ show x4
  t " est:"
  let prop5 = [ "y = " <> (if d4 == 0 then "" else showHead d4 "x")
                       <> showInline (a1 * x4 * x4 + b1 * x4 + c1 - d4 * x4) ""
              , "y = " <> (if d4 == 0 then "" else showHead (-d4) "x")
                       <> showInline (- a1 * x4 * x4 + b1 * x4 + c1 - d4 * x4) ""
              , "y = " <> (if d4 == 0 then "" else showHead d4 "x")
                       <> showInline (b1 * x4 + c1 - d4 * x4) ""
              ]
      rep5 = [0]
      rs5 = rand $ rand $ rand rs4
      ord5 = unsort 3 rs4
  nl
  nl
  KaTeX.put $ showProp m prop5 ord5

  nl
  nl

  b "6. "
  let {a: a6, b: b6, c: c6, r: r6} = randTriple rs5
  t "Un vecteur directeur de la droite d'équation "
  m $ showCartesian {a: a6, b: b6, c: c6}
  t " est:"
  let prop6 = [vector a6 b6, vector (-b6) a6, vector b6 (-a6)]
      rep6 = [1,2]
      rs6 = rand $ rand $ rand r6
      ord6 = unsort 3 r6
  nl
  nl
  KaTeX.put $ showProp m prop6 ord6

  b "7. "
  t "Une équation cartésienne de la droite passant par le point "
  let rxa = rand rs6
      rsxa = rand rxa
      rya = rand rsxa
      rsya = rand rya
      xa = (if rsxa.val `mod` 2 == 0 then 1 else -1) * (rxa.val `mod` intMax)
      ya = (if rsya.val `mod` 2 == 0 then 1 else -1) * (rya.val `mod` intMax)
      rvx = randPositive rsya
      rsvx = rand rvx
      rvy = randPositive rsvx
      rsvy = rand rvy
      vx = (if rsvx.val `mod` 2 == 0 then 1 else -1) * (rvx.val `mod` intMax)
      vy = (if rsvy.val `mod` 2 == 0 then 1 else -1) * (rvy.val `mod` intMax)

  m $ "A(" <> show xa <> ";" <> show ya <> ")"
  t " et de vecteur normal "
  m $ vector vx vy
  t " est:"
  let a7 = vx
      b7 = vy
      c7 = - vx * xa - vy * ya
  let prop7 = [ showCartesian {a: a7, b: b7, c: c7}
              , showCartesian {a: -2*a7, b: -2*b7, c: -2*c7}
              , showCartesian {a: b7, b: -a7, c: c7}
              ]
      rep7 = [0,1]
      rs7 = rand $ rand $ rand rsvy
      ord7 = unsort 3 rsvy
  nl
  nl
  KaTeX.put $ showProp m prop7 ord7

  nl
  nl

  b "8. "
  let {a: a8, b: b8, c: c8, r: r8} = randTriple rs7
  t "La droite "
  m "(d)"
  t " a pour équation "
  m $ showCartesian {a: a8, b: b8, c: c8}
  t " et la droite "
  m "(d')"
  t " a pour équation "
  m $ showCartesian {a: -2*b8, b: 2*a8, c: 2*c8-1}
  t ". Alors la droite "
  m "(d')"
  t " est:"
  let prop8 = [ "\\mathrm{parallèle}\\;\\mathrm{à}\\;\\mathrm{la}\\;\\mathrm{droite}\\;(d)"
              , "\\mathrm{perpendiculaire}\\;\\mathrm{à}\\;\\mathrm{la}\\;\\mathrm{droite}\\;(d)"
              , "\\mathrm{sécante}\\;\\mathrm{à}\\;\\mathrm{la}\\;\\mathrm{droite}\\;(d)"
              ]
      rep8 = [1,2]
      rs8 = rand $ rand $ rand r8
      ord8 = unsort 3 r8
  nl
  nl
  KaTeX.put $ showProp m prop8 ord8

  nl
  nl

  b "9. "
  t "L'ensemble des points "
  m "M(x;y)"
  t " tels que "
  let rxo = randPositive rs8
      rsxo = rand rxo
      ryo = randPositive rsxo
      rsyo = rand ryo
      xo = (if rsxo.val `mod` 2 == 0 then 1 else -1) * (rxo.val `mod` intMax)
      yo = (if rsyo.val `mod` 2 == 0 then 1 else -1) * (ryo.val `mod` intMax)
      rrad = randPositive rsyo
      rad = rrad.val `mod` intMax
  m $ "(x" <> showInline (-xo) "" <> ")^2+(y" <> showInline (-yo) "" <> ")^2=" <> show (rad*rad)
  t ":"
  let prop9 = [ "\\mathrm{est}\\;\\mathrm{un}\\;\\mathrm{cercle}\\;\\mathrm{de}\\;\\mathrm{centre}\\;(" <> show xo <> ";" <> show (-yo) <> ")"
              , "\\mathrm{est}\\;\\mathrm{un}\\;\\mathrm{cercle}\\;\\mathrm{de}\\;\\mathrm{rayon}\\;" <> show rad
              , "\\mathrm{n'est}\\;\\mathrm{pas}\\;\\mathrm{un}\\;\\mathrm{cercle}\\;"
              ]
      rep9 = [1]
      rs9 = rand $ rand $ rand rrad
      ord9 = unsort 3 rrad
  nl
  nl
  KaTeX.put $ showProp m prop9 ord9

  nl
  nl

  b "10. "
  t "L'ensemble des points "
  m "M(x;y)"
  t " tels que "
  let rxp = rand rs9
      rsxp = rand rxp
      ryp = rand rsxp
      rsyp = rand ryp
      xp = (if rsxp.val `mod` 2 == 0 then 1 else -1) * (rxp.val `mod` intMax)
      yp = (if rsyp.val `mod` 2 == 0 then 1 else -1) * (ryp.val `mod` intMax)
      rrad2 = rand rsyp
      rsrad = rand rrad2
      rad2 = (if rsrad.val `mod` 2 == 0 then 1 else -1) * (rrad2.val `mod` intMax)
      c10 = rad2 - xp*xp - yp*yp
  m $ "x^2+y^2" <> showInline (-2*xp) "x"
                <> showInline (-2*yp) "y"
                <> showInline (-c10) "" <> "=0"
  t " est:"
  let prop10 = [ "\\mathrm{un}\\;\\mathrm{cercle}\\;\\mathrm{de}\\;\\mathrm{rayon}\\;\\mathrm{nul}\\;"
               , "\\mathrm{un}\\;\\mathrm{cercle}\\;\\mathrm{de}\\;\\mathrm{rayon}\\;\\mathrm{non}\\;\\mathrm{nul}\\;"
               , "\\mathrm{vide}\\;"
               ]
      rep10 = case unit of
                unit | rad2 < 0 -> [2]
                     | rad2 == 0 -> [0]
                     | otherwise -> [1]
      rs10 = rand $ rand $ rand rsrad
      ord10 = unsort 3 rsrad
  nl
  nl
  KaTeX.put $ showProp m prop10 ord10


  nl
  nl
  if model.code < 0
    then do
          nl
          t "réponses: "
          t "1. "
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
