module Main where

import Prelude

import Data.Array ((!!)) as Array
import Data.Array (filter, length, mapWithIndex, tail, zip, (..))
import Data.Const (Const)
import Data.Foldable (foldr, sum)
import Data.Int (floor, toNumber)
import Data.List (head, toUnfoldable)
import Data.Map (Map, empty, insert, isEmpty, lookup, values)
import Data.Map (filter) as Map
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Rational (Rational)
import Data.Rational (fromInt) as Q
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Global (readFloat)
import Global.Unsafe (unsafeEncodeURIComponent)
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

quantize :: Int -> Number -> Number
quantize n x = (toNumber $ 2 * level n x + 1) / toNumber n - 1.0

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

type Probability = Rational
type Singleton = {option :: Int, probability :: Probability}
type Couple = Tuple Singleton Singleton
data Conditional = Singleton Singleton | Couple Couple

instance showConditional :: Show Conditional where
  show (Singleton s) = "Singleton" <> show s
  show (Couple c) = "Couple" <> show c

predicateConditional :: (Probability -> Boolean) -> Conditional -> Boolean
predicateConditional f (Singleton s) = f s.probability
predicateConditional f (Couple (Tuple s1 s2)) =
  f $ s1.probability + s2.probability

defaultConditional :: Conditional -> Boolean
defaultConditional = predicateConditional (_ < Q.fromInt 1)

excessConditional :: Conditional -> Boolean
excessConditional = predicateConditional (_ > Q.fromInt 1)

completeConditionals :: Map Int Conditional -> Boolean
completeConditionals cs =
  (isEmpty $ Map.filter defaultConditional cs)
  && (isEmpty $ Map.filter excessConditional cs)

-- |
-- | Organize a discrete n-sized law of probability as an n-sized array
-- | of singletons or couples, more suited for drawing. For instance
-- |
-- |  A    B    C    D    =>       I          II             III       IV
-- | 0.2  0.3  0.1  0.4  abs:  (D:0.25) (D:0.15, C:0.1) (B:0.25) (B:0.05, A:0.2)
-- |                     rel:  (D:1)   (D:3/5, C:2/5)    (B:1)   (B:1/5, A:4/5)
-- |
-- | so that P(A) = P(A|IV)P(IV) = (4/5)(1/4) = 1/5
-- |         P(B) = P(B|III)P(III)+P(B|IV)P(IV) = (1)(1/4)+(1/5)(1/4) = 3/10
-- |         P(C) = P(C|II)P(II) = (2/5)(1/4) = 1/10
-- |         P(D) = P(D|I)P(I)+P(D|II)P(II) = (1)(1/4)+(3/5)(1/4) = 2/5
-- |

reshpeDiscreteDistribution :: Array Probability -> Array Conditional
reshpeDiscreteDistribution ps =
  let n = length $ filter (_ /= Q.fromInt 0) ps
      initial =
        foldr (\(Tuple i p) m ->
          if p > Q.fromInt 0
            then insert i
                   (Singleton { option: i
                              , probability: p * Q.fromInt n})
                   m
            else m) empty
            $ mapWithIndex (\i p -> Tuple i p) ps
      go cur =
        if completeConditionals cur
          then cur
          else
            let kexcess = unsafePartial $ fromJust $ head
                          $ keys $ Map.filter excessConditional cur
                kdefault =  unsafePartial $ fromJust $ head
                          $ keys $ Map.filter defaultConditional cur
                cexcess = case lookup kexcess cur of
                  Just (Singleton s) -> Just s
                  _                  -> Nothing
                cdefault = case lookup kdefault cur of
                  Just (Singleton s) -> Just s
                  _                  -> Nothing
                next = maybe empty identity
                  $ (\e d ->
                      let diff = Q.fromInt 1 - d.probability
                      in insert kexcess (Singleton { option: e.option
                                                   , probability: e.probability - diff
                                                   })
                        $ insert kdefault (Couple
                          $ Tuple d
                                  { option: e.option
                                  , probability: diff
                                  }) cur) <$> cexcess <*> cdefault
            in go next
  in toUnfoldable $ values $ go initial

process :: FileContent -> FileContent
process str = foldr (\a b -> show a <> "\n" <> b) ""
  $ (\x -> [x])
  $ reshpeDiscreteDistribution
  <$> (buildMap
  $ (level order)
    <$> readFloat
    <$> (filter (_ /= "") $ split (Pattern "\n") str))

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
