module Numeric.Calculus where

import Prelude

import Data.Array (length, zipWith, (..))
import Data.Complex (Cartesian(..), real, pow)
import Data.Complex.FFT (Direction(..), fft, fft2)
import Data.Int (toNumber)
import Math (pi)

type TimeResolution = Number
type FrequencyResolution = Number

type Signal1D =
  { dT :: TimeResolution
  , samples :: Array Number
  }

frequencyResolution :: forall a. {dT :: Number, samples :: Array a} -> FrequencyResolution
frequencyResolution xs = 1.0 / (xs.dT * toNumber (length xs.samples - 1))

type Order = Int

differentiate :: Order -> Signal1D -> Signal1D
differentiate n xs =
  let nSamples = length xs.samples
      fe = frequencyResolution xs
      magicCoef i = pow (Cartesian 0.0 ((_*(2.0*pi*fe)) $
                              toNumber (if i <= nSamples `div` 2
                                            then i
                                            else i - nSamples)))
                        (toNumber n)
  in { dT: xs.dT
     , samples: real <$> (fft Backward $ zipWith mul (magicCoef <$>
                          (0 .. (nSamples-1))) $
                              fft Forward $
                                 (\x -> Cartesian x 0.0) <$> xs.samples)
     }

type Signal2D =
  { dT :: TimeResolution
  , samples :: Array (Array Number)
  }

grid :: forall a b c f. Functor f => (a -> b -> c) -> f a -> f b -> f (f c)
grid f xs ys = (\y -> (\x -> f x y) <$> xs) <$> ys

mapmap :: forall a b f. Functor f => (a -> b) -> f (f a) -> f (f b)
mapmap = (<$>) <<< (<$>)

gradient :: Order -> Order -> Signal2D -> Signal2D
gradient m n xss =
  let nSamples = length xss.samples
      fe = frequencyResolution xss
      magic k p = pow (Cartesian 0.0 ((_*(2.0*pi*fe)) $
                              toNumber (if k <= nSamples `div` 2
                                            then k
                                            else k - nSamples)))
                        (toNumber p)
      magicCoef i j = magic i m * magic j n
  in { dT: xss.dT
     , samples: real `mapmap` (fft2 Backward $ zipWith (zipWith mul)
                        (grid magicCoef (0 .. (nSamples-1)) (0 .. (nSamples-1)))
                          (fft2 Forward $
                                 (\x -> Cartesian x 0.0) `mapmap` xss.samples))
     }

laplacian :: Signal2D -> Signal2D
laplacian xss =
  { dT: xss.dT
  , samples: zipWith (zipWith add) (gradient 2 0 xss).samples (gradient 0 2 xss).samples
  }
