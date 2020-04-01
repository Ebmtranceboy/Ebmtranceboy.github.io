module Data.Complex.FFT where

import Prelude
import Data.String (toCodePointArray, singleton, length)
import Data.Array (reverse, replicate, mapWithIndex)
import Data.Int (toStringAs, binary, fromStringAs, toNumber)
import Data.Int.Bits (shl)
import Data.Foldable (foldr)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)
import Data.Complex (Cartesian(..), conjugate)
import Math (sqrt, cos, atan2, pi)
import PRNG ((!!))
import Control.Monad.ST (ST)
import Data.Array.ST (STArray, push, pushAll, peek, poke, empty, run)
import Control.Monad.Rec.Class (Step(..), tailRec)

type Index = Int
type PowerOfTwo = Int
type ExponentOfTwo = Int

initialSort :: Index -> ExponentOfTwo -> Index
initialSort i e2 =
  let s2' = toStringAs binary i
      s2 =  foldr (<>) "" (replicate (e2 - length s2') "0") <> s2'
      c2 = singleton <$> (reverse $ toCodePointArray s2)
  in unsafePartial $ fromJust $ fromStringAs binary $ foldr (<>) "" c2

data Direction = Forward | Backward
type Freq = Int
type Bin = { re :: Number
           , im :: Number
           , freq :: Freq
           , mag :: Number
           , phase :: Number
           }

bin :: Freq -> Cartesian Number -> Bin
bin k (Cartesian re im) =
  { re
  , im
  , freq: k
  , mag: sqrt $ re * re + im * im
  , phase: atan2 im re
  }

organize :: forall a. ExponentOfTwo -> Array (Cartesian Number)
              -> ST a (STArray a (Cartesian Number))
organize b zs = tailRec go {idx: 0, arrM: empty}
  where go {idx, arrM} =
          if idx == shl 1 b
            then Done arrM
            else Loop { idx: idx+1
                      , arrM: do
                            arr <- arrM
                            _ <- push (zs !! (initialSort idx b)) arr
                            pure arr
                      }

trigTable :: forall a. PowerOfTwo -> ST a (STArray a (Cartesian Number))
trigTable n = tailRec go { idx: 0
                         , arrM: do
                            a <- empty
                            _ <- push (Cartesian 1.0 0.0) a
                            pure a
                         }
  where go {idx, arrM} =
          let csn = cos (pi / toNumber n)
              sn = sqrt (1.0 - csn * csn)
            in if idx == n
                then Done arrM
                else Loop { idx: idx + 1
                          , arrM: do
                              arr <- arrM
                              Cartesian x y <- unsafePeek idx arr
                              _ <- push (Cartesian (x*csn-y*sn) (y*csn+x*sn)) arr
                              pure arr
                          }

unsafePeek :: forall a. Int -> STArray a (Cartesian Number) -> ST a (Cartesian Number)
unsafePeek idx arr = do
  mz <- peek idx arr
  pure $ unsafePartial $ fromJust mz

process :: forall a. Direction -> ExponentOfTwo -> Array (Cartesian Number)
                     -> Array (Cartesian Number) -> ST a (STArray a (Cartesian Number))
process dir b fs complexp = tailRec goi { i: 0
                                        , arrM: do
                                                   a <- empty
                                                   _ <- pushAll fs a
                                                   pure a
                                        }
  where gok j count step {k, arrM} =
          if k == step
            then Done arrM
            else Loop { k: k+1
                      , arrM: do
                                arr <- arrM
                                let idx = 2*j*step+k
                                f0 <- unsafePeek idx arr
                                fk <- unsafePeek (idx+step) arr
                                let cexp = complexp !! (count*k)
                                let f1 = fk * (case dir of
                                                  Forward -> conjugate cexp
                                                  Backward -> cexp)
                                _ <- poke idx (f0+f1) arr
                                _ <- poke (idx+step) (f0-f1) arr
                                pure arr
                      }
        goj count step {j, arrM} =
          if j == count
            then Done arrM
            else Loop {j: j+1, arrM: tailRec (gok j count step) {k: 0, arrM}}
        goi {i, arrM} =
          if i == b
            then Done arrM
            else
              let count = shl 1 (b-i-1)
                  step = shl 1 i
              in Loop {i: i+1, arrM: tailRec (goj count step) {j: 0, arrM}}

fft :: Array (Cartesian Number) -> ExponentOfTwo -> Direction -> Array Bin
fft zs b dir =
  let n = shl 1 b
      fs = run (organize b zs)
      complexp = run (trigTable (n `div` 2))
   in (\{index, value} -> bin index value) <$> (
        mapWithIndex (\index value -> {index, value}) $
          run (process dir b fs complexp)
          )
