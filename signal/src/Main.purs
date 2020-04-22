module Main where

import Prelude

import Control.Monad.ST (ST, run, for)
import Data.Array ((!!)) as Array
import Data.Array (filter, length, replicate, tail, take, zip, (..))
import Data.Array.ST (STArray, poke, withArray)
import Data.Complex (Cartesian(..), real)
import Data.Complex.FFT (Bin, Complex, Direction(..), fft, sortByMagnitude, unsafePeek)
import Data.Const (Const)
import Data.Foldable (foldr, sum)
import Data.Int (floor, toNumber)
import Data.Map (Map, empty, insert, lookup, values)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Rational (fromInt) as Q
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global (readFloat)
import Global.Unsafe (unsafeEncodeURIComponent)
import PRNG (Conditional(..), Probability, draw, prng, randoms, reshpeDiscreteDistribution)
import Partial.Unsafe (unsafePartial)
import Spork.App as App
import Spork.Html (Html, InputType(..))
import Spork.Html as H
import Spork.Interpreter (merge, never, throughAff)
import Web.DOM.Element (Element, setAttribute)
import Web.Event.Event (Event, target)
import Web.File.File (toBlob)
import Web.File.FileList (item)
import Web.File.FileReader.Aff (readAsText)
import Web.HTML.HTMLElement (click, fromElement)
import Web.HTML.HTMLInputElement (files, fromEventTarget)

nth :: forall a. Array a -> Int -> a
nth xs i =  unsafePartial fromJust $ xs Array.!! i

infixl 6 nth as !!

type FileContent = String

level :: Int -> Number -> Int
level n x =
  let candidate = floor $ toNumber n * (x + 1.0) / 2.0
  in if candidate >= n
      then n - 1
      else candidate

order = 13  :: Int

rescale :: Int -> Int -> Number
rescale n lev =  (toNumber $ 2 * lev + 1) / toNumber n - 1.0

quantize :: Int -> Number -> Number
quantize n x = rescale n (level n x)

buildMap :: Array Int -> Map (Tuple Int Int) (Array Probability)
buildMap x0s =
  let x1s = unsafePartial $ fromJust $ tail x0s
      x2s = unsafePartial $ fromJust $ tail x1s
      xs = zip (zip x0s x1s) x2s
  in probabilize <$> foldr (\(Tuple t@(Tuple x0 x1) x2) m ->
    let ys = case lookup t m of
              Just zs -> zs
              _       -> empty
        ys' = case lookup x2 ys of
                Just z -> insert x2 (z+1) ys
                _      -> insert x2 1 ys
    in insert t ys' m
    ) empty xs

probabilize :: Map Int Int -> Array Probability
probabilize m =
  let s = Q.fromInt $ sum $ values m
  in (\i -> case lookup i m of
                  Just j -> if s > Q.fromInt 0
                              then Q.fromInt j / s
                              else Q.fromInt 1 / Q.fromInt order
                  _      -> if s > Q.fromInt 0
                              then Q.fromInt 0
                              else Q.fromInt 1 / Q.fromInt order
    ) <$> (0..(order-1))

buildStochastic :: forall h. Map (Tuple Int Int) (Array Conditional) -> Array Int -> Int -> STArray h Int -> ST h Unit
buildStochastic arr rnds n a =
  for 2 (n-1) \ i -> do
    n0 <- unsafePeek (i-2) a
    n1 <- unsafePeek (i-1) a
    let r0 = rnds !! (2*i)
    let r1 = rnds !! (2*i+1)
    let n2 = draw (case lookup (Tuple n0 n1) arr of
                    Just ps -> ps
                    _       -> [Singleton {option: order `div` 2, probability: Q.fromInt 1}]) r0 r1
    poke i n2 a

generateStochastic :: Int -> Map (Tuple Int Int) (Array Conditional) -> Array Number
generateStochastic n arr =
  let rnds = randoms (2*n) $ prng {gen: 0, val: 0, seed: 20200421}
    in rescale order <$> run (withArray (buildStochastic arr rnds n) (replicate n zero))

buildSpectral :: forall h. Array Bin -> STArray h Complex -> ST h Unit
buildSpectral bs a =
  for 0 (length bs) \i -> do
    let bin = bs !! i
    let k = bin.freq
    poke k (Cartesian bin.re bin.im) a

generateSpectral :: (Array Number) -> Int -> Array Bin -> Array Number
generateSpectral signal n bs =
  {-zipWith sub signal-} (real <$> (fft Backward $ run (withArray (buildSpectral bs) (replicate n zero))))

processSpectral :: FileContent -> FileContent
processSpectral str =
  let signal = readFloat  <$> (filter (_ /= "") $ split (Pattern "\n") str)
  in foldr (\a b -> show a <> "\n" <> b) ""
  $ generateSpectral signal 16384
  $ take 2000
  $ (sortByMagnitude
  $ fft Forward
  $ (\r -> Cartesian r 0.0)
  <$> signal)

processStochastic :: FileContent -> FileContent
processStochastic str = foldr (\a b -> show a <> "\n" <> b) ""
  -- $ (\x -> [x])
  $ generateStochastic 16384
  $ reshpeDiscreteDistribution
  <$> (buildMap
  $ (level order)
    <$> readFloat
    <$> (filter (_ /= "") $ split (Pattern "\n") str))

process :: FileContent -> FileContent
process = processSpectral

data Unpure a
    = GetFileText Event (FileContent -> a)
    | WriteFile Keys a

type State =
  { link :: Maybe (H.ElemRef Element)
  , filename :: Maybe String
  , textFile :: FileContent
  }

type Keys =
  { element :: Element
  , name :: String
  , text :: FileContent
  }

data Action = UpdateFileText Event
            | DoneReading FileContent
            | None
            | RegisterAnchor (H.ElemRef Element)
            | RegisterFilename String
            | UpdateText
            | DoWrite

update ∷ State -> Action -> App.Transition Unpure State Action
update s (UpdateFileText ev) =
  { model: s
  , effects: App.lift (GetFileText ev DoneReading)
  }
update s (DoneReading text) = App.purely $ s{textFile = text}
update s None = App.purely s
update s (RegisterAnchor ref) = App.purely $ s{link = Just ref}
update s (RegisterFilename name) = App.purely $ s{filename = Just name}
update s UpdateText = App.purely $ s{textFile = process s.textFile}
update s DoWrite =
     let
      effects = case s.link of
        Just (H.Created element) →
                App.lift (WriteFile { element
                                    , text: s.textFile
                                    , name: unsafePartial $ fromJust s.filename
                                    } None)
        _                        → mempty
    in
      { model: s, effects }

render ∷ State → Html Action
render s =
  H.div [] $
  [ H.input [H.type_ H.InputFile, H.onChange $ H.always UpdateFileText]
  , H.button [H.onClick $ H.always_ UpdateText] [H.text "Process"]
  , H.input [H.type_ InputText, H.onValueChange $ H.always RegisterFilename]
  , H.a [H.style "display=\"none\"", H.ref (H.always RegisterAnchor)] []
  ]
  <> maybe [] (const $ [ H.button [H.onClick $ H.always_ DoWrite]
                                  [H.text "Save"]
                       ]) s.filename

app ∷ App.App Unpure (Const Void) State Action
app = { update
      , subs: const mempty
      , render
      , init: App.purely  { link: Nothing
                          , filename: Nothing
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
        WriteFile s next -> liftEffect $ do
          setAttribute  "href"
                        ("data:text/plain;charset=utf-8,"
                            <> unsafeEncodeURIComponent s.text)
                        s.element
          setAttribute "download" s.name s.element
          click (unsafePartial $ fromJust $ fromElement s.element)
          pure next

main :: Effect Unit
main = do
    let interpreter = throughAff runUnpure (const $ pure unit)
    inst <- App.makeWithSelector (interpreter `merge` never) app "#app"
    inst.run
