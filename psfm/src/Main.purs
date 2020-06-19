module Main where

import Prelude

import Color (rgb, cssStringRGBA)
import Concur.Core (Widget)
import Concur.VDom (HTML)
import Concur.VDom.Run (runWidgetInDom)
import Concur.VDom.SVG as S
import Data.Array (concat, length, mapWithIndex, zip, zipWith, (..))
import Data.Complex (Cartesian(..))
import Data.Either (Either(..), hush)
import Data.Foldable (sum, foldr)
import Data.Int (round, toNumber)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (infinity)
import Data.Number.Format (toStringWith, fixed)
import Data.Sparse.Matrix (Matrix(..), (^))
import Data.String (drop, split, trim, Pattern(..), joinWith)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Handles (_type, onChange, onClick) as P
import Math (exp, pow)
import Nodes (label, text, div', input, button) as D
import Numeric.Calculus (Signal1D, Signal2D, differentiate, laplacian)
import PRNG ((!!)) 
import Parser.Eval (eval)
import Parser.Parser (parse)
import Parser.Syntax (Dual(..), Expr(..), tanh)
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

m1 = Matrix {width: 2, height: 3, coefficients: 1.0^0^0+2.0^1^1+3.0^2^1} :: Matrix Number
m2 = Matrix {height: 3, width: 2, coefficients: 4.0^0^0+5.0^1^1+6.0^2^1} :: Matrix Number

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

type State =
  { signal :: Signal1D
  , diff :: Signal1D
  , diff2 :: Signal1D
  , sig2D :: Signal2D
  , diff2D :: Signal2D
  , event :: Maybe Event
  }

data Action = Iterate

epsilon = 0.001 :: Number

add2D :: Signal2D -> Signal2D -> Signal2D
add2D {dT, samples: s1} {dT: dT_, samples: s2} = {dT, samples: zipWith (zipWith add) s1 s2}

mul2D :: Signal2D -> Signal2D -> Signal2D
mul2D  {dT, samples: s1} {dT: dT_, samples: s2} = {dT, samples: zipWith (zipWith mul) s1 s2}

scl2D :: Number -> Signal2D -> Signal2D
scl2D k  {dT, samples} = {dT, samples: (\xs -> (_* k) <$> xs) <$> samples}

bar :: OffsetY -> Number -> Number -> Widget HTML State
bar offsetY x y =
  let offsetX = 10.0
      scaleX = 0.50
      scaleY = 50.0
  in S.line
    [ S.unsafeMkProp "x1" (offsetX+scaleX*x)
    , S.unsafeMkProp "x2" (offsetX+scaleX*x)
    , S.unsafeMkProp "y1" (offsetY)
    , S.unsafeMkProp "y2" (offsetY-scaleY*y)
    , S.stroke "#000000"
    , S.strokeWidth 1
    ]
    []

type OffsetY = Number

plot1D :: OffsetY -> Array Number -> Array (Widget HTML State)
plot1D off xs =  mapWithIndex (\i x -> bar off (toNumber i) x) xs

charter :: Number -> String
charter v =
  let bounded = 255.0 * tanh (v/0.7)
  in if bounded < 0.0
    then cssStringRGBA $ rgb (255 - round (-bounded)) (255 - round (-bounded)) 255
    else cssStringRGBA $ rgb 255 (255 - round bounded) (255 - round bounded)

cell :: Number -> Number -> Number -> Widget HTML State
cell x y v =
  let unitX = 5.0
      unitY = 5.0
      fill = charter v
      m a b = "M " <> show (a*unitX) <> " " <> show (b*unitY) <> " "
      l a b = "L " <> show (a*unitX) <> " " <> show (b*unitY) <> " "
   in S.path
      [ S.stroke "#000000"
      , S.strokeWidth 0
      , S.fill fill
      , S.d $ m x y <> l (x+unitX) y <> l (x+unitX) (y+unitY) <> l x (y+unitY) <> "Z"
      ]
      []

plot2D :: Array (Array Number) -> Array ( Widget HTML State)
plot2D xys = concat $ mapWithIndex (\i xs -> mapWithIndex (\j v -> cell (toNumber i) (toNumber j) v) xs) xys


extremums :: Array Number -> {max :: Number, min :: Number}
extremums xs =
  { max: foldr max (-infinity) xs
  , min: foldr min infinity xs
  }

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

readWiget ::  State -> Widget HTML State
readWiget st = do
  mfs <- liftEffect $ maybe (pure Nothing) files (fromEventTarget =<< target =<< st.event)
  txt <- liftAff $ maybe (pure "") readAsText
                      $ (Just <<< toBlob) =<< item 0 =<< mfs
  st' <- D.div'
    [ D.input [P._type "file", (\ev -> st{event = Just ev}) <$> P.onChange]
    , D.label [] [D.text txt]
    , D.label [] [D.text $  (show $ extremums st.signal.samples) <> (show $ extremums st.diff.samples)]
    , D.button [(const $ st{sig2D = st.sig2D `add2D` scl2D epsilon (laplacian st.sig2D)}) <$> P.onClick]
                [D.text "Iterate"]
    , S.svg
        [ S.width "1280"
        , S.height "768"
        ]
      (
       --plot1D 100.0 (st.signal.samples)
       -- <> plot1D 300.0 (st.diff.samples)
       -- <> plot1D 500.0 (st.diff2.samples)
      --plot2D st.sig2D.samples
      plot2D st.sig2D.samples
      )

    ]
  readWiget st'

main :: Effect Unit
main = do
  runWidgetInDom "main" $ readWiget { signal: sigGaussian
                      , diff: differentiate 1 sigGaussian
                      , diff2: differentiate 2 sigGaussian
                      , sig2D: sigGaussian2D
                      , diff2D: laplacian sigGaussian2D
                      , event: Nothing
                      }

{-
main :: Effect Unit
main = do
  log $ show $ system1 []
  log $ show $ system2 ["hello"]
  log $ show $ system3 []

  log $ showMatrix $ dot m1 (transpose m2)
  log $ showMatrix $ dot m2 (transpose m1)

-}
