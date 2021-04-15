module Bivariate where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Math (sin, cos, sqrt, exp, log, asin, acos, atan) as Math

newtype Bi 
  = Bi
    { d0 :: Number
    , d1 :: Number /\ Number
    , d2 :: (Number /\ Number) /\ (Number /\ Number)
    }

derive instance eqBi :: Eq Bi
derive instance ordBi :: Ord Bi

instance showBi :: Show Bi where
    show (Bi b) = show b.d0

instance semiringBi :: Semiring Bi where
  add (Bi b1) (Bi b2) = Bi $ b1 + b2
  zero = Bi { d0: 0.0, d1: 0.0 /\ 0.0, d2: (0.0 /\ 0.0) /\  (0.0 /\ 0.0) }
  one = Bi { d0: 1.0, d1: 0.0 /\ 0.0, d2: (0.0 /\ 0.0) /\  (0.0 /\ 0.0) }
  mul (Bi b1) (Bi b2) 
    = Bi
      { d0: b1.d0 * b2.d0
      , d1: (gx1 * b2.d0 + b1.d0 * gx2) /\  (gy1 * b2.d0 + b1.d0 * gy2)
      , d2: ((b1.d0 * hxx2 + b2.d0 * hxx1 + 2.0 * gx1 * gx2 )
          /\ (b1.d0 * hxy2 + b2.d0 * hxy1 + gx1 * gy2 + gx2 * gy1))
        /\ (( b1.d0 * hyx2 + b2.d0 * hyx1 + gy1 * gx2 + gy2 * gx1) 
          /\ (b1.d0 * hyy2 + b2.d0 * hyy1 + 2.0 * gy1 * gy2))
      }
      where
      gx1 /\ gy1 = b1.d1
      gx2 /\ gy2 = b2.d1
      (hxx1 /\ hxy1) /\ (hyx1 /\ hyy1) = b1.d2
      (hxx2 /\ hxy2) /\ (hyx2 /\ hyy2) = b2.d2
      
instance ringBi :: Ring Bi where
  sub (Bi b1) (Bi b2) = Bi $ b1 - b2

instance divRingBi :: DivisionRing Bi where
  recip (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi
      { d0: 1.0 / d0
      , d1: (-gx / d0 / d0) /\ (-gy / d0 / d0)
      , d2: (((2.0 * gx * gx - d0 * hxx) / u3)
          /\ ((2.0 * gx * gy - d0 * hxy) / u3))
         /\ (((2.0 * gy * gx - d0 * hyx) / u3)
          /\ ((2.0 * gy * gy - d0 * hyy) / u3))
      }
      where
      u3 = d0 * d0 * d0

instance coomutativeRingBi :: CommutativeRing Bi

instance euclideanRingBi :: EuclideanRing Bi where
  div x y = x * recip y
  degree _ = 0
  mod _ _ = zero
  
bix :: Number -> Bi
bix x = Bi { d0: x, d1: 1.0 /\ 0.0, d2: (0.0 /\ 0.0) /\  (0.0 /\ 0.0) }

biy :: Number -> Bi
biy y = Bi { d0: y, d1: 0.0 /\ 1.0, d2: (0.0 /\ 0.0) /\  (0.0 /\ 0.0) }

bic :: Number -> Bi
bic r =  Bi { d0: r, d1: 0.0 /\ 0.0, d2: (0.0 /\ 0.0) /\  (0.0 /\ 0.0) }

class Real r where
  exp :: r -> r
  sqrt :: r -> r
  log :: r -> r
  sin :: r -> r
  cos :: r -> r
  tan :: r -> r
  cosh :: r -> r
  sinh :: r -> r
  tanh :: r -> r
  asin :: r -> r
  acos :: r -> r
  atan :: r -> r
  asinh :: r -> r
  acosh :: r -> r
  atanh :: r -> r
  
instance realBi :: Real Bi where
  exp (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi 
      { d0: e
      , d1: (gx * e) /\  (gy * e)
      , d2: ((e * (gx * gx + hxx)) /\ (e * (gx * gy + hxy)))
          /\((e * (gy * gx + hyx)) /\ (e * (gy * gy + hyy)))
      }
      where
      e = Math.exp d0
  
  sqrt (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi 
      { d0: s
      , d1: (gx / s / 2.0) /\  (gy / s / 2.0)
      , d2: (((2.0 * hxx * d0 - gx * gx) / 4.0 / d0 / s)
          /\ ((2.0 * hxy * d0 - gx * gy) / 4.0 / d0 / s))
         /\ (((2.0 * hyx * d0 - gy * gx) / 4.0 / d0 / s)
          /\ ((2.0 * hyy * d0 - gy * gy) / 4.0 / d0 / s))
      }
      where
      s = Math.sqrt d0
  
  log (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi 
      { d0: Math.log d0
      , d1: (gx / d0) /\  (gy / d0)
      , d2: (((hxx * d0 - gx * gx) / d0 / d0)
          /\ ((hxy * d0 - gx * gy) / d0 / d0))
         /\ (((hyx * d0 - gy * gx) / d0 / d0)
          /\ ((hyy * d0 - gy * gy) / d0 / d0))
      }
      
  sin (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi 
      { d0: s
      , d1: (gx * c) /\  (gy * c)
      , d2: ((hxx * c - gx * gx * s)
          /\ (hxy * c - gx * gy * s))
         /\ ((hyx * c - gy * gx * s)
          /\ (hyy * c - gy * gy * s))
      }
      where
      s = Math.sin d0
      c = Math.cos d0
      
  cos (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi 
      { d0: c
      , d1: (-gx * s) /\  (-gy * s)
      , d2: ((-hxx * s - gx * gx * c)
          /\ (-hxy * s - gx * gy * c))
         /\ ((-hyx * s - gy * gx * c)
          /\ (-hyy * s - gy * gy * c))
      }
      where
      s = Math.sin d0
      c = Math.cos d0
      
  tan b = sin b / cos b
  cosh b = (exp b + exp (-b)) / bic 2.0
  sinh b = (exp b - exp (-b)) / bic 2.0
  tanh b = sinh b / cosh b
  
  asin (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi 
      { d0:  Math.asin d0
      , d1: (gx / r) /\  (gy / r)
      , d2: (((hxx * r + gx * gx * d0 / r) / m)
          /\ ((hxy * r + gx * gy * d0 / r) / m))
         /\ (((hyx * r + gy * gx * d0 / r) / m)
          /\ ((hyy * r + gy * gy * d0 / r) / m))
      }
      where
      m = 1.0 - d0 * d0
      r = Math.sqrt m
      
  acos (Bi b)
    = Bi 
      { d0: Math.acos b.d0
      , d1: -d1
      , d2: -d2
      }
      where
      Bi { d0, d1, d2} = asin (Bi b)
      
  atan (Bi { d0, d1: gx /\ gy, d2: (hxx /\ hxy) /\ (hyx /\ hyy) }) 
    = Bi 
      { d0:  Math.atan d0
      , d1: (gx / p) /\  (gy / p)
      , d2: (((hxx * p - 2.0 * gx * gx * d0) / p / p)
          /\ ((hxy * p - 2.0 * gx * gy * d0) / p / p))
         /\ (((hyx * p - 2.0 * gy * gx * d0) / p / p)
          /\ ((hyy * p - 2.0 * gy * gy * d0) / p / p))
      }
      where
      p = 1.0 + d0 * d0
      
  asinh b = log $ b + (sqrt $ b * b + bic 1.0)
  acosh b = log $ b + (sqrt $ b * b - bic 1.0)
  atanh b = bic 0.5 * (log $ (bic 1.0 + b) / (bic 1.0 - b))

 
  
