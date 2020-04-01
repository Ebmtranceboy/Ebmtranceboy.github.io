module ML.LinAlg where

import Prelude hiding (mul,add)

import Control.Apply (lift2)
import Data.Array (concat, (..))
import Data.Foldable (foldr, sum)
import Data.Int (toNumber)
import Data.Map (Map, empty, foldSubmap, fromFoldable, insert, lookup, mapMaybe, toUnfoldable, unionWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Math (exp) as Math
import Prelude (mul, add) as Prelude

data Direction = Forward | Backward

nonLinSigmoid :: Direction -> Number -> Number
nonLinSigmoid Forward x = 1.0 / (1.0 + Math.exp (-x))
nonLinSigmoid Backward x = x * (1.0 - x)

type Matrix =
  { nrows :: Int
  , ncols :: Int
  , content :: Map (Tuple Int Int) Number
  }

ins :: Int -> Int -> Number -> Map (Tuple Int Int) Number
      -> Map (Tuple Int Int) Number
ins i j v = insert (Tuple i j) v

showMatrix :: Matrix -> String
showMatrix m =
  joinWith "\n" $ (\i -> joinWith " " $ (\j ->
    maybe "0" show (lookup (Tuple i j) m.content))
     <$> (0 .. (m.ncols - 1)))
     <$> (0 .. (m.nrows - 1))

-- | (Non-commutative) Matrix product
dot :: Matrix -> Matrix -> Matrix
dot m1 m2 =
  let inter = m1.ncols -- == m2.nrows
      coef i j = sum $ maybe 0.0 identity <$>
                (\k -> lift2 Prelude.mul (lookup (Tuple i k) m1.content)
                                         (lookup (Tuple k j) m2.content))
                  <$> (0 .. (inter-1))
      content = foldr (\(Tuple key val) map -> insert key val map) empty $
              concat ((\j -> (\i -> Tuple (Tuple i j) $ coef i j)
                  <$> (0 .. (m1.nrows - 1)))
                  <$> (0 .. (m2.ncols - 1)))
    in {nrows: m1.nrows, ncols: m2.ncols, content}

transpose :: Matrix -> Matrix
transpose {nrows, ncols, content} =
  { nrows: ncols
  , ncols: nrows
  , content: fromFoldable $ (\(Tuple (Tuple i j) v) -> Tuple (Tuple j i) v)
            <$> (toUnfoldable content :: Array (Tuple (Tuple Int Int) Number))
  }

lift1Matrix :: (Number -> Number) -> Matrix -> Matrix
lift1Matrix f {nrows, ncols, content} =
  { nrows
  , ncols
  , content: mapMaybe (\v -> Just $ f v) content
  }

mean :: Matrix -> Number
mean {nrows, ncols, content} =
  let Additive sum = foldSubmap Nothing Nothing (\_ v -> Additive v) content
  in sum / toNumber (nrows * ncols)

lift2Matrix :: (Number -> Number -> Number) -> Matrix -> Matrix -> Matrix
lift2Matrix f m1@{nrows, ncols, content} m2 =
  { nrows
  , ncols
  , content: unionWith f content m2.content
  }

add :: Matrix -> Matrix -> Matrix
add = lift2Matrix Prelude.add

mul :: Matrix -> Matrix -> Matrix
mul m1 m2 = add (lift2Matrix (\ a b -> (a+1.0)*(b+1.0)-1.0) m1 m2)
                (lift1Matrix (\x -> - x) $ add m1 m2)
