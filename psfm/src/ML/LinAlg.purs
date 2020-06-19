module ML.LinAlg where

import Prelude

import Math (exp) as Math

data Direction = Forward | Backward

nonLinSigmoid :: Direction -> Number -> Number
nonLinSigmoid Forward x = 1.0 / (1.0 + Math.exp (-x))
nonLinSigmoid Backward x = x * (1.0 - x)
