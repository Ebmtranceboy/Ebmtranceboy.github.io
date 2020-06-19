module ML.LinAlg where

import Prelude

import Data.Array (foldr, (..))
import Data.Int (toNumber)
import Data.Sparse.Matrix (Matrix(..), height, transpose, width, (??), (^))
import Effect.Random (random)
import Effect.Unsafe (unsafePerformEffect)
import Math (abs)
import Math (exp) as Math
--import PRNG ((!!))

data Direction = Forward | Backward

nonLinSigmoid :: Direction -> Number -> Number
nonLinSigmoid Forward x = 1.0 / (1.0 + Math.exp (-x))
nonLinSigmoid Backward x = x * (1.0 - x)

input = Matrix { height: 4, width: 3
    , coefficients:  1.0^0^0+1.0^0^1
                    +1.0^1^1+1.0^1^2
                    +1.0^2^0+1.0^2^2
                            +1.0^3^2} :: Matrix Number
output = Matrix { height: 4, width: 1, coefficients: 1.0^0^0+1.0^0^1+1.0^0^3} :: Matrix Number

mean :: Matrix Number -> Number
mean m@(Matrix {height, width, coefficients}) =
  let left = Matrix { height: 1
                    , width: height
                    , coefficients: foldr (\i acc -> acc + 1.0^0^i) zero $ 0..(height - 1)
                    }
      right = Matrix { height: width
                     , width: 1
                     , coefficients: foldr (\i acc -> acc + 1.0^i^0) zero $ 0..(width - 1)
                     }
  in (_ / (toNumber width * toNumber height)) $ (left * m * right) ?? [0,0]

times :: Matrix Number -> Matrix Number -> Matrix Number
times m1 m2 =
  let h = height m1
      w = width m1
  in Matrix { height: h
            , width: w
            , coefficients:
                foldr (\i ai ->
                  foldr (\j aj ->
                    aj + (m1??[i,j] * m2??[i,j])^i^j) ai
                      $ 0 .. (w-1)) zero
                    $ 0 .. (h-1)
            }

randMatrix :: Int -> Int -> Matrix Number
randMatrix h w =
     Matrix { height: h
              , width: w
              , coefficients:
                  foldr (\i ai ->
                    foldr (\j aj ->
                      aj + (unsafePerformEffect random)^i^j) ai
                        $ 0 .. (w-1)) zero
                      $ 0 .. (h-1)
              }

epsilon = 1e-3 :: Number

predict :: Matrix Number -> Matrix Number -> { alpha :: Matrix Number, omega :: Matrix Number}
predict x y =
  let h = height x
      w = width x -- == height y
      loop s0 s1 =
        let l1 = nonLinSigmoid Forward <$> x * s0
            l2 = nonLinSigmoid Forward <$> l1 * s1
            e2 = y - l2
            d2 = times e2 $ nonLinSigmoid Backward <$> l2
            e1 = d2 * transpose s1
            d1 = times e1 $ nonLinSigmoid Backward <$> l1
          in if mean (abs <$> e2) < epsilon
              then {alpha: s0, omega: s1}
              else loop (s0 + transpose x * d1) (s1 + transpose l1 * d2)
      in loop (randMatrix w h) (randMatrix h 1)
