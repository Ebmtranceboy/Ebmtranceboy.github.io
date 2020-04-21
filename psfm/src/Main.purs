module Main where

import Prelude

import Color (rgb, cssStringRGBA)
import Data.Array (length, mapWithIndex, zip, (..), concat, zipWith)
import Data.Complex (Cartesian(..))
import Data.Const (Const)
import Data.Either (Either(..), hush)
import Data.Foldable (sum, foldr)
import Data.Int (round, toNumber)
import Data.Map (fromFoldable, empty)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Number (infinity)
import Data.Number.Format (toStringWith, fixed)
import Data.String (drop, split, trim, Pattern(..), joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import ML.LinAlg (Matrix, ins)
import Math (exp, pow)
import Numeric.Calculus (Signal1D, Signal2D, differentiate, laplacian)
import PRNG ((!!))
import Parser.Eval (eval)
import Parser.Parser (parse)
import Parser.Syntax (Dual(..), Expr(..), tanh)
import Partial.Unsafe (unsafePartial)
import SVGpork.Render (svgline, svgpath)
import Spork.App as App
import Spork.Html (Html)
import Spork.Html as H
import Spork.Interpreter (merge, never, throughAff)
import Web.Event.Event (Event, target)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.File.FileReader.Aff (readAsText)
import Web.HTML.HTMLInputElement (files, fromEventTarget)

type System a = Array a -> Array a

liftExprDual :: Number -> Expr Dual
liftExprDual x = Lit $ Dual {height: x, slope: 1.0}

execute :: Maybe (Expr Dual) -> Array String -> Array Number -> Expr Dual
execute f vars vals =
  case f of
    Just expr ->
      case eval (fromFoldable (zip vars $ liftExprDual <$> vals)) expr of
        Right exp -> exp
        _ -> Var "undefined"
    _ -> Var "undefined"

unused = 0.0 :: Number

image :: Maybe (Expr Dual) -> Array String -> Array Number -> Number
image f vars vals =
  case execute f vars vals of
    Lit (Dual {height: value, slope}) -> value
    _ -> unused

source1 :: System Int
source1 = \_ -> [1, 2]

mixer1 :: System Int
mixer1 arr = [sum arr]

f1 :: System Int
f1 arr = [- (arr !! 0), - (arr !! 1)]

system1 :: System Int
system1 = source1 >>> f1 >>> mixer1

source2 :: System String
source2 arr = let str = arr !! 0 in [str, str <> str]

mixer2 :: System String
mixer2 = \arr -> [(arr !! 0) <> (arr !! 1)]

f2 :: System String
f2 arr = [drop 1 (arr !! 0), drop 2 (arr !! 1)]

system2 :: System String
system2 = source2 >>> f2 >>> mixer2

names :: forall a. Array a -> Array String
names arr = (\n -> "x" <> show n) <$> 1 .. length arr

numberSystem :: String -> System Number
numberSystem command = \ arr ->
  let commands = trim <$> split (Pattern ",") command
      nbFromCommand cmd =
        image (hush $ Right cmd >>= parse) (names arr) arr
    in nbFromCommand <$> commands

alteredNumberSystem :: String -> (Expr Dual -> Expr Dual) -> System Number
alteredNumberSystem command modifier = \ arr ->
  let commands = trim <$> split (Pattern ",") command
      nbFromCommand cmd =
        image (hush $ Right cmd >>= (\c -> modifier <$> parse c)) (names arr) arr
    in nbFromCommand <$> commands

source3 = numberSystem "1.5, 2.7" ::  System Number

mixer3 :: System Number
mixer3 arr = [sum arr]

deepRename :: String -> String -> Expr Dual -> Expr Dual
deepRename _ _ e@(Lit _) = e
deepRename from to e@(Var name) = if name == from then Var to else e
deepRename from to (Binop op e1 e2) = Binop op (deepRename from to e1) (deepRename from to e2)
deepRename from to (Unop op e) = Unop op (deepRename from to e)

f3 = alteredNumberSystem "x1+1.0, x2+2.0, 3" (deepRename "x2" "x1") :: System Number

system3 :: System Number
system3 = source3 >>> f3 >>> mixer3

m1 = {nrows: 3, ncols: 2, content: ins 0 0 1.0 $ ins 1 1 2.0 $ ins 2 1 3.0 empty} :: Matrix
m2 = {nrows: 3, ncols: 2, content: ins 0 0 4.0 $ ins 1 1 5.0 $ ins 2 1 6.0 empty} :: Matrix

class Formatted a where
  showWithFormat :: Int -> a -> String

instance formattedNumber :: Formatted Number where
  showWithFormat n x = toStringWith (fixed n) x

instance formattedComplex :: Formatted (Cartesian Number) where
  showWithFormat n (Cartesian a b) =
    showWithFormat n a
    <> (if b<zero then "-" <> showWithFormat n (negate b) else "+" <> showWithFormat n b) <> "i"

instance formattedArray :: Formatted a => Formatted (Array (Array a)) where
  showWithFormat n xxs =
    "[" <> joinWith ",\n" ((\xs -> "[" <> joinWith ", " (showWithFormat n <$> xs) <> "]") <$> xxs) <> "]"

type FileContent = String

data Unpure a
    = GetFileText Event (FileContent -> a)

type State =
  { signal :: Signal1D
  , diff :: Signal1D
  , diff2 :: Signal1D
  , sig2D :: Signal2D
  , diff2D :: Signal2D
  , textFile :: FileContent
  }

data Action = Iterate
            | UpdateFileText Event
            | DoneReading FileContent

epsilon = 0.001 :: Number

add2D :: Signal2D -> Signal2D -> Signal2D
add2D {dT, samples: s1} {dT: dT_, samples: s2} = {dT, samples: zipWith (zipWith add) s1 s2}

mul2D :: Signal2D -> Signal2D -> Signal2D
mul2D  {dT, samples: s1} {dT: dT_, samples: s2} = {dT, samples: zipWith (zipWith mul) s1 s2}

scl2D :: Number -> Signal2D -> Signal2D
scl2D k  {dT, samples} = {dT, samples: (\xs -> (_* k) <$> xs) <$> samples}

update ∷ State -> Action -> App.Transition Unpure State Action
update s Iterate = App.purely $ s{sig2D = s.sig2D `add2D` scl2D epsilon (laplacian s.sig2D)}
update s (UpdateFileText ev) =
  { model: s
  , effects: App.lift (GetFileText ev DoneReading)
  }
update s (DoneReading text) = App.purely $ s{textFile = text}

bar :: OffsetY -> Number -> Number -> Html Action
bar offsetY x y =
  let offsetX = 10.0
      scaleX = 0.50
      scaleY = 50.0
  in svgline (offsetX+scaleX*x) (offsetY) (offsetX+scaleX*x) (offsetY-scaleY*y) "#000" 0.2

type OffsetY = Number

plot1D :: OffsetY -> Array Number -> Array (Html Action)
plot1D off xs =  mapWithIndex (\i x -> bar off (toNumber i) x) xs

charter :: Number -> String
charter v =
  let bounded = 255.0 * tanh (v/0.7)
  in if bounded < 0.0
    then cssStringRGBA $ rgb (255 - round (-bounded)) (255 - round (-bounded)) 255
    else cssStringRGBA $ rgb 255 (255 - round bounded) (255 - round bounded)

cell :: Number -> Number -> Number -> Html Action
cell x y v =
  let unitX = 5.0
      unitY = 5.0
      fill = charter v
      m a b = "M " <> show (a*unitX) <> " " <> show (b*unitY) <> " "
      l a b = "L " <> show (a*unitX) <> " " <> show (b*unitY) <> " "
   in svgpath "#000" 0.0 fill $ m x y <> l (x+unitX) y <> l (x+unitX) (y+unitY) <> l x (y+unitY) <> "Z"


plot2D :: Array (Array Number) -> Array (Html Action)
plot2D xys = concat $ mapWithIndex (\i xs -> mapWithIndex (\j v -> cell (toNumber i) (toNumber j) v) xs) xys

extremums :: Array Number -> {max :: Number, min :: Number}
extremums xs =
  { max: foldr max (-infinity) xs
  , min: foldr min infinity xs
  }

render ∷ State → Html Action
render s =
  H.div []
  [ H.label [] [H.text $  (show $ extremums s.signal.samples) <> (show $ extremums s.diff.samples)]
  , H.button [H.onClick $ H.always_ Iterate] [H.text "Iterate"]
  , H.input [H.type_ H.InputFile, H.onChange $ H.always UpdateFileText]
  , H.label [] [H.text $ s.textFile]
  , H.elemWithNS
      ns
      "svg"
      [ H.attr "width" "1280"
      , H.attr "height" "768"
      ]
      (
       {-plot1D 100.0 (s.signal.samples)
       <> plot1D 300.0 (s.diff.samples)
       <> plot1D 500.0 (s.diff2.samples) -}
      --plot2D s.sig2D.samples
      plot2D s.sig2D.samples

      )
  ]
    where
      ns = Just $ H.Namespace "http://www.w3.org/2000/svg"

nSamples = 64 :: Int

gaussian :: Array Number
gaussian = (\i -> exp (- pow ((toNumber $ i - nSamples `div` 2) / 10.0) 2.0)) <$> (0..(nSamples-1))

sigGaussian = {dT: 0.1, samples: gaussian} :: Signal1D

gaussian2D :: Int -> Int -> Number
gaussian2D i j = exp (- pow ((toNumber $ i - nSamples `div` 2) / 4.0) 2.0
                      - pow ((toNumber $ j - nSamples `div` 2) / 4.0) 2.0)

gaussians :: Array (Array Number)
gaussians = (\i -> (\j -> gaussian2D (i+4) (j-7) + gaussian2D (i-6) (j+5)) <$> (0..(nSamples-1))) <$> (0..(nSamples-1))

sigGaussian2D = {dT: 0.1, samples: gaussians} :: Signal2D

app ∷ App.App Unpure (Const Void) State Action
app = { update
      , subs: const mempty
      , render
      , init: App.purely  { signal: sigGaussian
                          , diff: differentiate 1 sigGaussian
                          , diff2: differentiate 2 sigGaussian
                          , sig2D: sigGaussian2D
                          , diff2D: laplacian sigGaussian2D
                          , textFile: ""
                          }
      }

runUnpure ::  Unpure ~> Aff
runUnpure unpure =
    case unpure of
        GetFileText ev next -> do
          mfs <- liftEffect $ files (unsafePartial
              $ fromJust $ fromEventTarget =<< target ev)
          next <$> (maybe (pure "") readAsText
                      $ (Just <<< toBlob) =<< item 0 =<< mfs)

main :: Effect Unit
main = do
    let interpreter = throughAff runUnpure (const $ pure unit)
    inst <- App.makeWithSelector (interpreter `merge` never) app "#app"
    inst.run

{-
main :: Effect Unit
main = do
  log $ show $ system1 []
  log $ show $ system2 ["hello"]
  log $ show $ system3 []

  log $ showMatrix $ dot m1 (transpose m2)
  log $ showMatrix $ dot m2 (transpose m1)

-}
