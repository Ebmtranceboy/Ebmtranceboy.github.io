module Main where

import Prelude

import Data.Array (zip, length, (..), take, (:), any, concat)
import Data.Foldable (sum)
import Data.Array (drop) as Array
import Data.Either (Either(..), hush)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (drop, split, trim, Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import MCMC.Utils (computeMCstats)
import Mecanism (System, (!!))
import Parser.Eval (eval)
import Parser.Parser (parse)
import Parser.Syntax (Dual(..), Expr(..))
import Rand (Rand, rands)
import Math (sqrt, pi, cos, sin)
import Math (log) as Math

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

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks n xs = case unit of
  unit | length xs <= n    -> [xs]
       | otherwise         -> take n xs : chunks n (Array.drop n xs)


uniforms :: Int -> Int -> Number -> Number -> Rand -> Array (Array Number)
uniforms d n a b r =
  let f x = a + (b - a) * (toNumber x / 10000.0)
  in chunks d $ f <$> rands (n*d-1) r

normals :: Int -> Int -> Number -> Number -> Rand -> Array (Array Number)
normals d n mu sigma rnd =
  let m = d*n + (d*n) `mod` 2
      us = uniforms 2 (m `div` 2) 0.00001 1.0 rnd
    in chunks d $ (\x -> x * sigma + mu) <$> (concat $ (\u ->
                      let r = sqrt (-2.0 * Math.log (u !! 0))
                      in [ r * cos (2.0 * pi * (u !! 1))
                         , r * sin (2.0 * pi * (u !! 1))] ) <$> us )

pop = 2000 :: Int
dim = 3 :: Int

norm :: Array Number -> Number
norm xs = sqrt $ sum $ (\x -> x * x) <$> xs

test1 :: String
test1 = show $ computeMCstats $ (\chunk ->
                  if any (\x -> x < -1.0 || x > 1.0) chunk
                    then 0.0
                    else 1.0) <$> uniforms dim pop (-3.0) 3.0 {val: 3, gen: 14, seed: 159265}

test2 :: String
test2 = show $ computeMCstats $ (\chunk ->
                  if norm chunk > 1.0
                    then 0.0
                    else 1.0) <$> normals dim pop 0.0 1.0 {val: 3, gen: 14, seed: 159265}

main :: Effect Unit
main = do
  log $ show $ system1 []
  log $ show $ system2 ["hello"]
  log $ show $ system3 []
  log test1
  log test2
