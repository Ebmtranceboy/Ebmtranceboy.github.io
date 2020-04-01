module Main where

import Prelude

import Data.Array (zip, length, (..))
import Data.Either (Either(..), hush)
import Data.Foldable (sum)
import Data.Map (fromFoldable, empty)
import Data.Maybe (Maybe(..))
import Data.String (drop, split, trim, Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import ML.LinAlg (Matrix, ins, showMatrix, dot, transpose)
import Parser.Eval (eval)
import Parser.Parser (parse)
import Parser.Syntax (Dual(..), Expr(..))
import Data.Complex.FFT (initialSort, fft, Direction(..))
import Data.Complex (Cartesian(..))
import Data.Traversable (traverse_)
import PRNG ((!!))

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

main :: Effect Unit
main = do
  log $ show $ system1 []
  log $ show $ system2 ["hello"]
  log $ show $ system3 []
  log $ showMatrix $ dot m1 (transpose m2)
  log $ showMatrix $ dot m2 (transpose m1)
  traverse_ (\x -> log $ show x) $ (flip initialSort 3) <$> [0,1,2,3,4,5,6,7]
  log $ show $ fft [ Cartesian 0.0 0.0
                   , Cartesian 1.0 0.0
                   , Cartesian 2.0 0.0
                   , Cartesian 3.0 0.0
                   , Cartesian 4.0 0.0
                   , Cartesian 5.0 0.0
                   , Cartesian 6.0 0.0
                   , Cartesian 7.0 0.0
                   ] 3 Forward
